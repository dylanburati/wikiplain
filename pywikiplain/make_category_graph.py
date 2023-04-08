from dataclasses import dataclass
from functools import partial
import itertools
import math
import operator
import pickle
import sys
import pyarrow.parquet as pq
from sqlalchemy import create_engine
from sqlalchemy.sql import text as sqltext
from tqdm import tqdm

@dataclass
class Context:
    enwiki_parquet_filename: str
    pqf_size: int
    old_database_uri: str
    new_database_uri: str
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
    """Transforms a Wikipedia `categorylinks` table into a graph of article IDs."""
    engine_c = create_engine(ctx.old_database_uri)
    engine_c.execution_options(yield_per=10000)
    engine_cg = create_engine(ctx.new_database_uri)
    with (engine_c.connect() as conn_c,
          engine_cg.connect() as conn_cg
    ):
        cr_stmt = sqltext(
            """CREATE TABLE categorylinks (
                cl_from INTEGER NOT NULL,
                cl_to INTEGER NOT NULL,
                cl_subcat BOOLEAN NOT NULL
            )"""
        )
        ins_stmt = sqltext("INSERT INTO categorylinks (cl_from, cl_to, cl_subcat) VALUES (:cl_from, :cl_to, :cl_subcat)")
        conn_cg.execute(cr_stmt)

        res = conn_c.execute(sqltext("SELECT cl_from, cl_to, cl_type FROM categorylinks"))
        for partition in tqdm(iter(partial(res.fetchmany, 10000), [])):
            records = []
            for aid, cat, typ in partition:
                if typ not in ("subcat", "page"):
                    continue
                cat = str(cat).replace('_', ' ')
                try:
                    rec = {
                        "cl_from": aid,
                        "cl_to": int(category_id_map[cat]),
                        "cl_subcat": typ == "subcat"
                    }
                    records.append(rec)
                except KeyError:
                    print(f"KeyError {(aid, cat, typ)!r}")
            if len(records) > 0:
                with conn_cg.begin():
                    conn_cg.execute(ins_stmt, records)
        
        conn_cg.execute(sqltext("CREATE INDEX idx_categorylinks_cl_from ON categorylinks (cl_from)"))
        conn_cg.execute(sqltext("CREATE INDEX idx_categorylinks_cl_to ON categorylinks (cl_to)"))

if __name__ == "__main__":
    ctx = Context(
        enwiki_parquet_filename=sys.argv[1],
        pqf_size=int(sys.argv[2]),
        old_database_uri=sys.argv[3],
        new_database_uri=sys.argv[4],
        enwiki_dir=sys.argv[5]
    )
    pqf = pq.ParquetFile(ctx.enwiki_parquet_filename)
    category_id_map = get_category_id_map(ctx, pqf)
    with open(ctx.category_id_map_filename, "wb") as fp:
        pickle.dump(category_id_map, fp)
    create_table(ctx, category_id_map)
