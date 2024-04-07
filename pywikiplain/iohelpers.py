import io
from types import TracebackType
from typing import IO, Type, Iterator, Iterable, Optional, List


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

