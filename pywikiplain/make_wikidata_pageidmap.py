from dataclasses import dataclass
import gzip
from io import BytesIO
import os
import sys

import cbor2
import httpx
from tqdm import tqdm
from iohelpers import ReadableIterator
import wikiplain


@dataclass
class Context:
    page_sql_url: str
    wikidata_dir: str

    @property
    def page_id_map_filename(self):
        return f"{self.wikidata_dir}/page_id_map.bin"

def create_map(ctx: Context) -> None:
    """Transforms a Wikidata `page` sql.gz dump into a (page.page_id -> page.page_title) map."""
    with open(ctx.page_id_map_filename, "wb") as outfile:
        with httpx.stream("GET", ctx.page_sql_url) as response:
            total = int(response.headers["Content-Length"])
            stream = ReadableIterator(response.iter_bytes())
            fp = gzip.open(stream, "rb")
            with tqdm(total=total, unit_scale=True, unit_divisor=1024, unit="B") as progress:
                stream_into_file(fp, outfile, progress, response)

def stream_into_file(fp: gzip.GzipFile, outfile: BytesIO, rprogress: tqdm, response: httpx.Response):
    with tqdm(position=1) as wprogress:
        num_bytes_downloaded = response.num_bytes_downloaded
        for line in fp:
            rprogress.update(response.num_bytes_downloaded - num_bytes_downloaded)
            num_bytes_downloaded = response.num_bytes_downloaded
            try:
                table_name, rows = wikiplain.sql.parse_insert_statement(line)  # type: ignore
            except ValueError:
                continue
            if table_name != "page":
                continue
            for r in rows:
                assert len(r) == 12
                # +--------------------+---------------------+------+
                # | Field              | Type                | Null |
                # +--------------------+---------------------+------+
                # | page_id            | int(10) unsigned    | NO   | [0]
                # | page_namespace     | int(11)             | NO   | [1]
                # | page_title         | varbinary(255)      | NO   | [2]
                # | page_is_redirect   | tinyint(3) unsigned | NO   | [3]
                # | page_is_new        | tinyint(3) unsigned | NO   | [4]
                # | page_random        | double unsigned     | NO   | [5]
                # | page_touched       | binary(14)          | NO   | [6]
                # | page_links_updated | varbinary(14)       | YES  | [7]
                # | page_latest        | int(10) unsigned    | NO   | [8]
                # | page_len           | int(10) unsigned    | NO   | [9]
                # | page_content_model | varbinary(32)       | YES  | [10]
                # | page_lang          | varbinary(35)       | YES  | [11]
                # +--------------------+---------------------+------+
                if r[1] == 0:
                    # main namespace
                    cbor2.dump((r[0], r[2]), outfile)
                    wprogress.update()
                    # note: this one seems to have been moved
                    # | page_restrictions  | tinyblob            | YES  | [3]


if __name__ == "__main__":
    ctx = Context(
        page_sql_url=sys.argv[1],
        wikidata_dir=sys.argv[2]
    )
    print(ctx)
    create_map(ctx)
