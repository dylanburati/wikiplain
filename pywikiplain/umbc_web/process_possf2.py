import glob
import itertools
import json
import sys
from concurrent.futures import as_completed, ProcessPoolExecutor
import traceback
from typing import Iterator

from tqdm import tqdm


PENN_TAGS_BY_ID = [
    "#",
    "$",
    ",",
    ".",
    "CC",
    "CD",
    "DT",
    "EX",
    "FW",
    "IN",
    "JJ",
    "JJR",
    "JJS",
    "LS",
    "MD",
    "NN",
    "NNP",
    "NNPS",
    "NNS",
    "PDT",
    "POS",
    "PRP",
    "PRP$",
    "RB",
    "RBR",
    "RBS",
    "RP",
    "SYM",
    "TO",
    "UH",
    "VB",
    "VBD",
    "VBG",
    "VBN",
    "VBP",
    "VBZ",
    "WDT",
    "WP",
    "WP$",
    "WRB",
    "<s>",
]

PENN_TAGS = {tag: id for id, tag in enumerate(PENN_TAGS_BY_ID)}


def preprocess(paragraph: str) -> Iterator[tuple[str, str]]:
    """Yields the word-tag pairs in a .possf2 format paragraph.
    
    Removes quote tokens and maps em-dashes and colons to commas.
    """
    tokens = iter(paragraph.split())
    for tok in tokens:
        pair = tok.rsplit("_", maxsplit=1)
        if len(pair) < 2:
            try:
                next(tokens)
            except StopIteration:
                break
        elif pair[1] in ("``", "''", "EOS"):
            pass
        elif pair[1] in ("-LRB-", "-RRB-", ":"):
            yield (pair[0], ",")
        else:
            word, tag = pair
            yield (word, tag)


def process(in_filename: str, out_filename: str) -> None:
    """Convert a .possf2 file into a line-delimited JSON file.

    Each line of the output file is a sentence (array); Each element of that
    array is a triple: [normalized: string, possessive: "" | "'" | "'s", tag: int]
    """
    with open(in_filename, "r", encoding="utf-8") as fp:
        text = fp.read()
    with open(out_filename, "w", encoding="utf-8") as fp:
        for para in text.split("\n\n"):
            acc: list[tuple[str, int]] = []
            for word, tag in preprocess(para):
                if tag == ".":
                    json.dump(acc, fp, separators=(',', ':'))
                    fp.write('\n')
                    acc = []
                else:
                    acc.append((word.lower(), PENN_TAGS[tag]))
            if len(acc) > 0:
                json.dump(acc, fp, separators=(',', ':'))
                fp.write('\n')


def try_process(in_filename: str, out_filename: str) -> None:
    """Wraps process to return an error that includes the input filename."""
    try:
        process(in_filename, out_filename)
    except Exception as exc:
        raise ValueError(in_filename) from exc


if __name__ == "__main__":
    in_filenames = glob.glob(sys.argv[1].rstrip("/") + "/*.possf2")
    with ProcessPoolExecutor(max_workers=10) as executor:
        futures = [
            executor.submit(process, fname, fname.replace(".possf2", ".ldjson"))
            for fname in in_filenames
        ]
        with tqdm(total=len(futures)) as progress:
            for f in as_completed(futures):
                try:
                    assert f.result() is None
                except Exception as e:
                    traceback.print_exception(e)
                progress.update()
