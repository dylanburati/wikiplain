import json
import os
from pathlib import Path
import re
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

import chartrie
import cbor2
import ijson
import numpy as np
import sqlalchemy as sa
from spacy.lang.en import English
from sqlalchemy.sql import select, text as sqltext
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
    trie = chartrie.CharTrie()
    for domain in top_cite_domain_set:
        domain_b = domain.encode()
        trie[domain_b] = 1
        trie[b"https://"+domain_b] = 1
        trie[b"http://"+domain_b] = 1
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
            url_s = submission.get("url")
            if url_s is None:
                continue
            title = submission.get("title")
            if title is None:
                continue
            if not trie.has_prefix_of(url_s.encode()):
                continue
            url = try_urlsplit(url_s)
            if url is None:
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
                if not (token.is_left_punct or token.is_right_punct or token.is_space)
            ]
            if len(tokens) > 0:
                yield tokens
            curr_time = time.time()
            if curr_time - progress_time > 1:
                progress_time = curr_time
                prog_file.write(f"{fp.tell()} {total_s}\n")
                prog_file.flush()

SPACE_REGEX = re.compile(r" ")

class SpanList:
    def __init__(self, phrase: str, max_span_map: dict[str, int]):
        self.phrase = phrase
        self.word_offsets = []
        if phrase:
            self.word_offsets.append(0)
            self.word_offsets.extend(m.start()+1 for m in SPACE_REGEX.finditer(phrase))
        self.word_offsets.append(len(phrase)+1)

        W = len(self.word_offsets) - 1
        self.length = W
        max_span_widths = np.zeros(W, dtype=np.int32)
        for i, (start, next_start) in enumerate(zip(self.word_offsets, self.word_offsets[1:])):
            word = phrase[start:next_start-1]
            if (width := max_span_map.get(word)) is not None:
                width = min(width, W - i)
                max_span_widths[i:i+width] = np.max(
                    [
                        max_span_widths[i:i+width],
                        width - np.arange(width, dtype=np.int32),
                    ],
                    axis=0
                )

        self.max_span_widths = max_span_widths
        self.pos = 0
        self.ww = max_span_widths[0] if W > 0 else 0
        self.min_span_ends = 1 + np.arange(W, dtype=np.int32)

    def all(self) -> Iterator[str]:
        for i, w in enumerate(self.max_span_widths):
            start = self.word_offsets[i]
            for j in range(w):
                end = self.word_offsets[i+j+1] - 1
                yield self.phrase[start:end]

    def next(self) -> tuple[int, int, str] | None:
        if self.pos >= self.length:
            return None

        while self.pos+self.ww < self.min_span_ends[self.pos]:
            self.pos += 1
            if self.pos >= self.length:
                return None
            self.ww = self.max_span_widths[self.pos]

        start = self.word_offsets[self.pos]
        end = self.word_offsets[self.pos+self.ww] - 1
        ww = self.ww
        self.ww -= 1
        return self.pos, ww, self.phrase[start:end]

    def mark_found(self, pos: int, width: int) -> None:
        self.min_span_ends[pos:pos+width] = np.max([
            self.min_span_ends[pos:pos+width],
            np.broadcast_to(pos+width, shape=(width,))
        ])


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
        for sentences in itertoolz.partition_all(
            250, submission_titles(pushshift_path, top_cite_domain_set)
        ):
            phrases = [" ".join(sentence) for sentence in sentences]
            tag_resp = pos_tagger.tag_sentences(sentences)
            span_lists = [SpanList(phrase, max_span_map) for phrase in phrases]
            terms = set()
            for span_list in span_lists:
                for term in span_list.all():
                    terms.add(term)

            weights_by_term: dict[str, list[tuple[int, float]]] = {}
            query_res: sa.CursorResult[tuple[str, int, float]] = conn.execute(
                select(term_map_sql.c.term, term_map_sql.c.id, term_map_sql.c.weight)
                .where(term_map_sql.c.weight >= 0.01)
                .where(term_map_sql.c.term.in_(list(terms)))
            )
            for term, node_id, weight in query_res:
                lst = weights_by_term.setdefault(term, [])
                lst.append((node_id, weight))

            for taginfo, sentence, span_list in zip(tag_resp, sentences, span_lists):
                # turn POS tags into noun-confidence score array (one element per word)
                scores = np.array([e["scores"] for e in taginfo])
                observations = np.array([e["observations"] for e in taginfo])
                priors = np.array([int(w.isalpha()) for w in sentence])
                scores -= scores.max(axis=1)[:, np.newaxis]
                scores *= 0.5
                np.exp(scores, out=scores)
                scores /= np.sum(scores, axis=1)[:, np.newaxis]
                score = np.max(scores[:, [PENN_TAGS["NNP"], PENN_TAGS["NNPS"]]], axis=1)
                score = (score * observations + priors * 20) / (observations + 20)

                while (item := span_list.next()) is not None:
                    pos, width, term = item
                    weights = weights_by_term.get(term)
                    if not weights:
                        continue
                    span_list.mark_found(pos, width)
                    noun_factor = score[pos:pos+width].mean()
                    if np.isnan(noun_factor):
                        raise ValueError(f"sentence={sentence!r}, item={item!r}")
                    for node_id, weight in weights:
                        relevance[node_id] += weight * noun_factor

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
    max_date = datetime.now() - timedelta(days=48)
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

    # compute_relevance(ctx, tasks[-1][0], tasks[-1][1])
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
