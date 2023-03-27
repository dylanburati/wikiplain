from enum import Enum
from typing import Optional

def load_bz2(dump_path: str, output_path: str) -> None:
    """Load a Wikimedia XML dump to into a parquet file."""

def test_parser(text: str) -> int:
    """Test the Wikitext tokenizer."""

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

    def __init__(self, kind: TokenKind, data: Optional[str]):
        ...

def tokenize(text: str) -> list[Token]:
    """Tokenize Wikitext."""

def get_links(text: str) -> list[str]:
    """Extract the internal links from a Wikitext string."""

def get_cite_urls(text: str) -> list[str]:
    """Extract the URLs used in web citations from a Wikitext string."""

def is_diambiguation_page(text: str) -> bool:
    """Detect whether the Wikitext string has a disambiguation marker template."""
