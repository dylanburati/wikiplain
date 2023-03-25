import math

import polars as pl
from ipywidgets import interact


def pager(DF, size):
    num_pages = math.ceil(DF.shape[0] / size)
    page_input = list(range(num_pages)) if num_pages < 1000 else (0, num_pages - 1, 1)
    return interact(lambda page: DF.slice(page*size, size), page=page_input)

def searcher(DF, columns, page_size):
    def searcher_run(q):
        mask = (
            DF
            .select([pl.col(c).str.contains(q) for c in columns])
            .max(axis=1)
        )
        return DF.filter(mask).slice(0, page_size)
    return interact(searcher_run, q="")
