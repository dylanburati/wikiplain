import io
import itertools
import json
import os
import pickle
import socket
import struct
import sys
import tempfile
import time
import traceback
from collections import defaultdict, deque
from concurrent.futures import as_completed, ProcessPoolExecutor
from dataclasses import dataclass
from datetime import date, datetime, time as timeofday, timedelta
from types import TracebackType
from typing import IO, Type, Iterator, Iterable, Optional, List, TypedDict, TypeVar
from urllib.parse import urlsplit, SplitResult as SplitURL
from xml.sax.saxutils import unescape as xml_unescape

import httpx
import ijson
import numpy as np
import sqlalchemy as sa
from sqlalchemy.sql import select, text as sqltext
from spacy.lang.en import English
from toolz import itertoolz
from tqdm import tqdm
from zstandard import ZstdDecompressor

from umbc_web.process_possf2 import PENN_TAGS


T1 = TypeVar("T1")
T2 = TypeVar("T2")


def lazy_product(iterable1: Iterable[T1], iterable2: Iterable[T2]) -> Iterator[tuple[T1, T2]]:
    """Version of itertools.product that doesn't fully load the iterators."""
    iter2 = iter(iterable2)
    for elem1 in iterable1:
        iter2, iter2_copy = itertools.tee(iter2)
        for elem2 in iter2_copy:
            yield (elem1, elem2)


class ReadableIterator(IO[bytes]):
    """File-like wrapper for an iterator that yields bytes."""

    __inner: Optional[Iterator[bytes]]
    __position: int
    __buffered: bytes

    def __init__(self, inner: Iterator[bytes]):
        self.__inner = inner
        self.__position = 0
        self.__buffered = b""

    def __enter__(self) -> IO[bytes]:
        return self

    def __exit__(
        self,
        __t: Optional[Type[BaseException]],
        __value: Optional[BaseException],
        __traceback: Optional[TracebackType],
    ) -> None:
        self.close()

    def close(self) -> None:
        """
        Close the IO object.

        Attempting any further operation after the object is closed will raise an OSError. This method has no
        effect if the file is already closed.
        """
        self.__inner = None

    def fileno(self) -> int:
        """
        Returns the underlying file descriptor (an integer).
        """
        return -1

    def readable(self) -> bool:
        """
        Returns True if the IO object can be read.
        """
        return True

    def __require_inner(self) -> Iterator[bytes]:
        if self.__inner is None:
            raise OSError("Can't read a closed file")
        return self.__inner

    def read(self, size: int = -1) -> bytes:
        """
        Read at most size bytes, returned as a bytes object.

        If the size argument is negative, read until EOF is reached.
        Return an empty bytes object at EOF.
        """
        if size == 0:
            return b""
        result = self.__buffered
        while size < 0 or len(result) < size:
            try:
                result += next(self.__require_inner())
            except StopIteration:
                break
        if size > 0:
            self.__buffered = result[size:]
            self.__position += size
            return result[:size]
        self.__buffered = b""
        self.__position += len(result)
        return result

    def readline(self, __limit: int = -1) -> bytes:
        raise ValueError("Line-based methods are not available on ReadableIterator")

    def readlines(self, __hint: int = -1) -> List[bytes]:
        raise ValueError("Line-based methods are not available on ReadableIterator")

    def seekable(self) -> bool:
        return False

    def seek(self, __offset: int, __whence: int = io.SEEK_CUR) -> int:
        raise ValueError("Cannot seek")

    def tell(self) -> int:
        return self.__position

    def writable(self) -> bool:
        return False

    def flush(self) -> None:
        raise ValueError("Cannot write")

    def truncate(self, __size: Optional[int] = None) -> int:
        raise ValueError("Cannot write")

    def write(self, __s: bytes) -> int:
        raise ValueError("Cannot write")

    def writelines(self, __lines: Iterable[bytes]) -> None:
        raise ValueError("Cannot write")

    def isatty(self) -> bool:
        return False

    def __next__(self) -> bytes:
        raise ValueError("Iterable methods are not available on ReadableIterator")

    def __iter__(self) -> Iterator[bytes]:
        raise ValueError("Iterable methods are not available on ReadableIterator")


Span = tuple[int, int]
RENORMALIZATIONS = {"|": "-"}


def try_urlsplit(url_str) -> Optional[SplitURL]:
    try:
        return urlsplit(url_str)
    except ValueError:
        return None


def submission_titles(
    pushshift_url: str, top_cite_domain_set: set[str]
) -> Iterator[list[str]]:
    """Yields tokenized titles of Reddit submissions in the Pushshift archive set.

    Only submissions whose URL is one of the given domains are considered.
    """
    nlp = English()
    task_id = pushshift_url.split("/")[-1]
    with (
        httpx.stream("GET", pushshift_url) as response,
        tempfile.NamedTemporaryFile(
            "w", encoding="utf-8", suffix=task_id + ".progress"
        ) as prog_file,
    ):
        total_s = response.headers.get("content-length")
        fp = ReadableIterator(response.iter_bytes())
        # redirect progress bar to a BytesIO and manually write progress
        dctx = ZstdDecompressor(max_window_size=2 * 1024 * 1024 * 1024)
        reader = dctx.stream_reader(fp)
        dedup_q: deque[tuple[str, str]] = deque()
        dedup_set: set[tuple[str, str]] = set()
        progress_time = 0.0
        for submission in ijson.items(reader, "", multiple_values=True):
            if "url" not in submission or "title" not in submission:
                continue
            url = try_urlsplit(submission["url"])
            if url is None or url.netloc not in top_cite_domain_set:
                continue
            dedup_key = (url.netloc, url.path)
            if dedup_key in dedup_set:
                continue
            if len(dedup_q) > 1000:
                dedup_set.discard(dedup_q.popleft())
            dedup_q.append(dedup_key)
            dedup_set.add(dedup_key)
            title = xml_unescape(submission["title"])
            tokens = [
                RENORMALIZATIONS.get(token.norm_, token.norm_)
                for token in nlp.tokenizer(title)
                if not (token.is_left_punct or token.is_right_punct)
            ]
            if len(tokens) > 0:
                yield tokens
            curr_time = time.time()
            if curr_time - progress_time > 1:
                progress_time = curr_time
                prog_file.write(f"{fp.tell()} {total_s}\n")
                prog_file.flush()


def compute_spans(
    tokens: list[str], max_span_map: dict[str, int]
) -> list[tuple[int, int]]:
    """Returns the (start_index, end_index) spans over `tokens` that can match Wikipedia titles."""
    spans = []
    for i, word0 in enumerate(tokens):
        max_size = min(len(tokens) - i, max_span_map.get(word0, 0))
        for j in range(i + 1, i + max_size + 1):
            spans.append((i, j))
    return spans


@dataclass
class Context:
    num_articles: int
    database_uri: str
    pos_tagger_port: int
    enwiki_dir: str

    @property
    def top_cite_domains_filename(self) -> str:
        return f"{self.enwiki_dir}/pagerank/top_cite_domains.pkl"


def load_max_span_map(conn: sa.Connection) -> dict[str, int]:
    """Loads the map from each word to max-length entry that starts with it."""
    result: dict[str, int] = {}
    for k, v in conn.execute(sqltext("SELECT k, v FROM max_span_map")):
        result[k] = v
    return result


class TagResult(TypedDict):
    """Part-of-speech tagging result."""

    scores: list[float]
    observations: int


def add_queries(
    sentence: list[str],
    spans: list[tuple[int, int]],
    taginfo: list[TagResult],
    queries: list[tuple[float, list[int]]],
    query_rev: defaultdict[str, list[int]],
) -> None:
    scores = np.array([e["scores"] for e in taginfo])
    observations = np.array([e["observations"] for e in taginfo])
    priors = np.array([int(w.isalpha()) for w in sentence])
    scores -= scores.max(axis=1)[:, np.newaxis]
    scores *= 0.5
    np.exp(scores, out=scores)
    scores /= np.sum(scores, axis=1)[:, np.newaxis]
    score = np.max(scores[:, [PENN_TAGS["NNP"], PENN_TAGS["NNPS"]]], axis=1)
    score = (score * observations + priors * 20) / (observations + 20)
    span_dict = {}
    for i, j in spans:
        qid = len(queries)
        span_score = score[i:j].mean()
        if span_score >= 0.01:
            span_dict[i, j] = qid
            k = " ".join(sentence[i:j])
            queries.append((span_score, []))
            query_rev[k].append(qid)
    for (i, j), qid in span_dict.items():
        if j - i > 1:
            children = []
            if (i, j - 1) in span_dict:
                children.append(span_dict[i, j - 1])
            if (i + 1, j) in span_dict:
                children.append(span_dict[i + 1, j])
            span_score, _ = queries[qid]
            queries[qid] = (span_score, children)


def compute_relevance(
    context: Context, pushshift_url: str, output_filename: str
) -> None:
    relevance = np.zeros(context.num_articles, dtype=np.float64)
    engine = sa.create_engine(context.database_uri)
    term_map_sql = sa.Table(
        "term_map",
        sa.MetaData(),
        sa.Column("term", sa.String),
        sa.Column("id", sa.Integer),
        sa.Column("weight", sa.Float),
    )
    with open(context.top_cite_domains_filename, "rb") as fp:
        top_cite_domains = pickle.load(fp)
        excluded_domains = {
            "imgur.com",
            "twitter.com",
            "youtube.com",
            "soundcloud.com",
            "instagram.com",
            "amazon.com",
            "github.com",
            "vimeo.com",
            "google.com",
        }
        top_cite_domain_set = {domain for domain, _ in top_cite_domains}
        top_cite_domain_set -= excluded_domains

    with engine.connect() as conn, socket.socket() as sock:
        max_span_map = load_max_span_map(conn)
        sock.connect(("127.0.0.1", context.pos_tagger_port))
        rfile = sock.makefile("rb", buffering=0)
        wfile = sock.makefile("wb", buffering=0)
        reader = ijson.items(rfile, "", use_float=True, multiple_values=True)
        for group in itertoolz.partition_all(
            50, submission_titles(pushshift_url, top_cite_domain_set)
        ):
            sentences = [json.dumps(e).encode("utf-8") + b"\n" for e in group]
            req = b"".join(sentences)
            wfile.write(struct.pack(">2I", len(req), 0) + req)
            resp: list[list[TagResult]] = next(reader)
            # Each element refers to a span in context; (prob of being a noun-phrase, children)
            #   - prob of being a noun-phrase is a simple average of P(max(token is NNP, token is NNPS))
            #     for the tokens in the query. Anything less than 0.01 is excluded
            #   - children is a list of indices pointing to the `length-1` subspans of this query,
            #     if this query's length is >1 and those subspans are actually queries
            queries: list[tuple[float, list[int]]] = []
            # Text of query -> indices in query list
            query_rev: defaultdict[str, list[int]] = defaultdict(list)
            for sentence, taginfo in zip(group, resp):
                spans = compute_spans(sentence, max_span_map)
                add_queries(sentence, spans, taginfo, queries, query_rev)

            query_res: sa.CursorResult[tuple[str, int, float]] = conn.execute(
                select(term_map_sql.c.term, term_map_sql.c.id, term_map_sql.c.weight)
                .where(term_map_sql.c.weight >= 0.01)
                .where(term_map_sql.c.term.in_(list(query_rev.keys())))
            )
            query_res_list = [row.tuple() for row in query_res]
            # If a query matched, queries generated from the same context which are contained by
            # that query should be ignored
            inner_matches: set[int] = set()
            for term, _, _ in query_res_list:
                for qid in query_rev[term]:
                    _, children = queries[qid]
                    stack = deque(children)
                    while len(stack):
                        curr = stack.pop()
                        inner_matches.add(curr)
                        _, grandchildren = queries[curr]
                        for c in grandchildren:
                            if c not in inner_matches:
                                stack.append(c)
            for term, node_id, weight in query_res_list:
                for qid in query_rev[term]:
                    if qid not in inner_matches:
                        relevance[node_id] += weight * queries[qid][0]

    with open(output_filename, "wb") as fp:
        pickle.dump(relevance, fp)


if __name__ == "__main__":
    ctx = Context(
        database_uri=sys.argv[1],
        num_articles=int(sys.argv[2]),
        pos_tagger_port=int(sys.argv[3]),
        enwiki_dir=sys.argv[4]
    )
    urls: list[str] = []
    filenames: list[str] = []
    max_date = datetime.now() - timedelta(days=45)
    for year, month in lazy_product(itertools.count(2015), range(1, 13)):
        if datetime.combine(date(year, month, 1), timeofday.min) >= max_date:
            break
        month_s = str(month).zfill(2)
        filename = f"{ctx.enwiki_dir}/pagerank/RS_{year}-{month_s}.pkl"
        if not os.path.exists(filename):
            urls.append(
                f"https://files.pushshift.io/reddit/submissions/RS_{year}-{month_s}.zst"
            )
            filenames.append(filename)
    with ProcessPoolExecutor(max_workers=3) as executor:
        futures = [
            executor.submit(compute_relevance, ctx, url, fname)
            for url, fname in zip(urls, filenames)
        ]
        with tqdm(total=len(futures)) as progress:
            for f in as_completed(futures):
                try:
                    assert f.result() is None
                except Exception as e:
                    traceback.print_exception(e)
                progress.update()
