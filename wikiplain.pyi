from enum import Enum
from typing import Optional

class TokenKind(Enum):
    Comment = ...
    TemplateStart = ...
    TemplateEnd = ...
    LinkStart = ...
    LinkEnd = ...
    ElementStart = ...
    ElementStartEnd = ...
    ElementEnd = ...
    Content = ...

class Token:
    kind: TokenKind
    data: Optional[str]

    def __init__(self, kind: TokenKind, data: Optional[str]): ...

class NodeKind(Enum):
    Document = ...
    Element = ...
    Template = ...
    Link = ...
    Argument = ...
    Content = ...

class Node:
    kind: NodeKind
    children: list[Node]
    data: Optional[str]

    def __init__(self, kind: NodeKind, children: list[Node], data: Optional[str]): ...

def load_bz2(dump_path: str, output_path: str) -> None:
    """Load a Wikimedia XML dump to into a parquet file."""

def test_parser(text: str) -> int:
    """Test the Wikitext tokenizer."""

def tokenize(text: str) -> list[Token]:
    """Tokenize Wikitext."""

def parse_tokens(tokens: list[Token]) -> Node:
    """Build a syntax tree from tokenized Wikitext."""

def parse(text: str) -> Node:
    """Tokenize Wikitext and build a syntax tree."""

def get_links(text: str) -> list[str]:
    """Extract the internal links from a Wikitext string."""

def get_cite_urls(text: str) -> list[str]:
    """Extract the URLs used in web citations from a Wikitext string."""

def is_diambiguation_page(text: str) -> bool:
    """Detect whether the Wikitext string has a disambiguation marker template."""

def get_first_infobox_title(text: str) -> str | None:
    """Get the title of the first infobox template in the Wikitext, or None if not found."""
