import json
import os
from pathlib import Path
import socket
import struct
import sys
import tempfile
import time
import traceback
from collections import defaultdict, deque
from concurrent.futures import as_completed, ProcessPoolExecutor
from contextlib import contextmanager
from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import Any, Iterator, Optional, TypedDict, cast
from urllib.parse import urlsplit, SplitResult as SplitURL
from xml.sax.saxutils import unescape as xml_unescape

import cbor2
import ijson
import numpy as np
import sqlalchemy as sa
from sqlalchemy.sql import select, text as sqltext
from spacy.lang.en import English
from toolz import itertoolz
from tqdm import tqdm
from zstandard import ZstdDecompressor

from umbc_web.process_possf2 import PENN_TAGS


Span = tuple[int, int]
RENORMALIZATIONS = {"|": "-"}


def try_urlsplit(url_str: str) -> Optional[SplitURL]:
    try:
        return urlsplit(url_str)
    except ValueError:
        return None


def submission_titles(
    pushshift_path: str, top_cite_domain_set: set[str]
) -> Iterator[list[str]]:
    """Yields tokenized titles of Reddit submissions in the Pushshift archive set.

    Only submissions whose URL is one of the given domains are considered.
    """
    nlp = English()
    task_id = Path(pushshift_path).stem
    with (
        open(pushshift_path, "rb") as fp,
        tempfile.NamedTemporaryFile(
            "w", encoding="utf-8", suffix=task_id + ".progress"
        ) as prog_file,
    ):
        total_s = os.stat(fp.fileno()).st_size
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
    reddit_dir: str

    @property
    def top_cite_domains_filename(self) -> str:
        return f"{self.enwiki_dir}/pagerank/top_cite_domains.bin"


class TagResult(TypedDict):
    """Part-of-speech tagging result."""

    scores: list[float]
    observations: int


class PartOfSpeechTagger:
    rfile: socket.SocketIO
    wfile: socket.SocketIO
    reader: Iterator[Any]

    def __init__(self, sock: socket.socket, port: int):
        sock.connect(("127.0.0.1", port))
        self.rfile = sock.makefile("rb", buffering=0)
        self.wfile = sock.makefile("wb", buffering=0)
        self.reader = ijson.items(self.rfile, "", use_float=True, multiple_values=True)

    def tag_sentences(self, sentences: list[list[str]]) -> list[list[TagResult]]:
        sentences_b = [json.dumps(e).encode("utf-8") + b"\n" for e in sentences]
        req = b"".join(sentences_b)
        self.wfile.write(struct.pack(">2I", len(req), 0) + req)
        return cast(list[list[TagResult]], next(self.reader))


@contextmanager
def new_pos_querier(port: int) -> Iterator[PartOfSpeechTagger]:
    with socket.socket() as sock:
        resource = PartOfSpeechTagger(sock, port)
        try:
            yield resource
        finally:
            resource.rfile.close()
            resource.wfile.close()


def load_max_span_map(conn: sa.Connection) -> dict[str, int]:
    """Loads the map from each word to max-length entry that starts with it."""
    result: dict[str, int] = {}
    for k, v in conn.execute(sqltext("SELECT k, v FROM max_span_map")):
        result[k] = v
    return result


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
    context: Context, pushshift_path: str, output_filename: str
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
        top_cite_domains = cbor2.load(fp)
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

    with engine.connect() as conn, new_pos_querier(context.pos_tagger_port) as pos_tagger:
        max_span_map = load_max_span_map(conn)
        for group in itertoolz.partition_all(
            50, submission_titles(pushshift_path, top_cite_domain_set)
        ):
            tag_resp = pos_tagger.tag_sentences(group)
            # queries: Each element refers to a span in context; (prob of being a noun-phrase, children)
            #   - prob of being a noun-phrase is a simple average of P(max(token is NNP, token is NNPS))
            #     for the tokens in the query. Anything less than 0.01 is excluded
            #   - children is a list of indices pointing to the `length-1` subspans of this query,
            #     if this query's length is >1 and those subspans are actually queries
            queries: list[tuple[float, list[int]]] = []
            # query_rev: Text of query -> indices in query list
            query_rev: defaultdict[str, list[int]] = defaultdict(list)
            for sentence, taginfo in zip(group, tag_resp):
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
        np.save(fp, relevance)


if __name__ == "__main__":
    ctx = Context(
        database_uri=sys.argv[1],
        num_articles=int(sys.argv[2]),
        pos_tagger_port=int(sys.argv[3]),
        enwiki_dir=sys.argv[4],
        reddit_dir=sys.argv[5]
    )
    tasks: list[tuple[str, str]] = []
    date_iterator = map(
        lambda e: datetime(year=e[0], month=e[1], day=1, hour=0, second=0),
        itertoolz.iterate(lambda e: (e[0] + e[1] // 12, 1 + e[1] % 12), (2015, 1))
    )
    max_date = datetime.now() - timedelta(days=45)
    for dt in date_iterator:
        if dt >= max_date:
            break
        ds = dt.isoformat()[:7]
        result_filename = f"{ctx.enwiki_dir}/pagerank/RS_{ds}.npy"
        if not os.path.exists(result_filename):
            tasks.append((
                f"{ctx.reddit_dir}/submissions/RS_{ds}.zst",
                result_filename,
            ))
    with ProcessPoolExecutor(max_workers=3) as executor:
        futures = [
            executor.submit(compute_relevance, ctx, pushshift, out_path)
            for pushshift, out_path in tasks
        ]
        with tqdm(total=len(futures)) as progress:
            for f in as_completed(futures):
                try:
                    assert f.result() is None
                except Exception as e:
                    traceback.print_exception(e)
                progress.update()
