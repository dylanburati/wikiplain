from dataclasses import dataclass
import gzip
import itertools
import math
import operator
import os
import pickle
import sys

import httpx
import pyarrow.parquet as pq
from sqlalchemy import create_engine
import sqlalchemy
from sqlalchemy.sql import text as sqltext
from tqdm import tqdm
from iohelpers import ReadableIterator
import wikiplain


@dataclass
class Context:
    enwiki_parquet_filename: str
    pqf_size: int
    categorylinks_url: str
    database_uri: str
    enwiki_dir: str

    @property
    def category_id_map_filename(self):
        return f"{self.enwiki_dir}/categories/category_id_map.pkl"

CATEGORY_PREFIX = "Category:"

def get_category_id_map(ctx: Context, pqf: pq.ParquetFile) -> dict[str, int]:
    """Creates a mapping from category name to article ID."""
    iterator = tqdm(pqf.iter_batches(batch_size=100), total=math.ceil(ctx.pqf_size / 100))
    iterator = map(
        lambda b: zip(
            b["id"].to_numpy(),
            b["ns"].to_numpy(),
            map(operator.attrgetter("is_valid"), b["redirect"]),
            b["title"].to_pylist(),
        ),
        iterator
    )
    iterator = itertools.chain.from_iterable(iterator)
    iterator = filter(lambda e: not e[2] and e[1] == 14, iterator)
    result = {}
    for aid, _, _, title in iterator:
        result[title[len(CATEGORY_PREFIX):]] = aid
    return result

def create_table(ctx: Context, category_id_map: dict[str, int]) -> None:
    """Transforms a Wikipedia `categorylinks` sql.gz dump into a graph of article IDs."""
    engine_cg = create_engine(ctx.database_uri)
    with engine_cg.connect() as conn_cg:
        cr_stmt = sqltext(
            """CREATE TABLE categorylinks (
                cl_from INTEGER NOT NULL,
                cl_to INTEGER NOT NULL,
                cl_subcat BOOLEAN NOT NULL
            )"""
        )
        conn_cg.execute(cr_stmt)

        with httpx.stream("GET", ctx.categorylinks_url) as response:
            total = int(response.headers["Content-Length"])
            stream = ReadableIterator(response.iter_bytes())
            fp = gzip.open(stream, "rb")
            with tqdm(total=total, unit_scale=True, unit_divisor=1024, unit="B") as progress:
                stream_into_table(fp, category_id_map, conn_cg, progress, response)
        conn_cg.execute(sqltext("CREATE INDEX idx_categorylinks_cl_from ON categorylinks (cl_from)"))
        conn_cg.execute(sqltext("CREATE INDEX idx_categorylinks_cl_to ON categorylinks (cl_to)"))

def stream_into_table(fp: gzip.GzipFile, category_id_map: dict[str, int], conn: sqlalchemy.Connection, progress: tqdm, response: httpx.Response):
    ins_stmt = sqltext("INSERT INTO categorylinks (cl_from, cl_to, cl_subcat) VALUES (:cl_from, :cl_to, :cl_subcat)")
    num_bytes_downloaded = response.num_bytes_downloaded
    for line in fp:
        progress.update(response.num_bytes_downloaded - num_bytes_downloaded)
        num_bytes_downloaded = response.num_bytes_downloaded
        records = []
        try:
            table_name, rows = wikiplain.parse_sql_insert_statement(line)
        except ValueError:
            continue
        if table_name != "categorylinks":
            continue
        for r in rows:
            assert len(r) == 7
            # cl_from, cl_to, cl_sortkey, cl_timestamp, cl_sortkey_prefix, cl_collation, cl_type
            if isinstance(r[1], str) and (typ := r[6]) in ("page", "subcat"):
                try:
                    records.append({
                        "cl_from": r[0],
                        "cl_to": int(category_id_map[r[1]]),
                        "cl_subcat": typ == "subcat"
                    })
                except KeyError:
                    print(f"KeyError {r!r}")
        if len(records) > 0:
            with conn.begin():
                conn.execute(ins_stmt, records)


if __name__ == "__main__":
    ctx = Context(
        enwiki_parquet_filename=sys.argv[1],
        pqf_size=int(sys.argv[2]),
        categorylinks_url=sys.argv[3],
        database_uri=sys.argv[4],
        enwiki_dir=sys.argv[5]
    )
    try:
        os.mkdir(f"{ctx.enwiki_dir}/categories")
    except FileExistsError:
        pass
    pqf = pq.ParquetFile(ctx.enwiki_parquet_filename)
    category_id_map = get_category_id_map(ctx, pqf)
    with open(ctx.category_id_map_filename, "wb") as fp:
        pickle.dump(category_id_map, fp)
    create_table(ctx, category_id_map)
