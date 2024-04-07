def parse_insert_statement(text: bytes) -> tuple[str, list[list[str | bytes | int | float | None]]]:
    """Parse a SQL insert statement and get the table name and list of rows to insert."""
