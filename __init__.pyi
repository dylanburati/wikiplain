from enum import Enum

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
    data: str | None

    def __init__(self, kind: TokenKind, data: str | None): ...

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
    data: str | None

    def __init__(self, kind: NodeKind, children: list[Node], data: str | None): ...

def load_bz2(dump_path: str, output_path: str) -> None:
    """Load a Wikimedia XML dump into a parquet file."""

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

def get_distinguish_hatnotes(text: str) -> list[str]:
    """Get the full template content for the 'For other uses ...' or 'This page is about _, ...' notes
    on the page.
    """

def get_first_infobox_title(text: str) -> str | None:
    """Get the title of the first infobox template in the Wikitext, or None if not found."""
