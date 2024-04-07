# pylint: disable
from collections.abc import Container, Sequence
from dataclasses import dataclass
from enum import Enum
from typing import Any, Callable, Generic, Literal, NoReturn, TypeVar

class ResultKind(Enum):
  OK = 0
  ERR = 1

E = TypeVar("E")
I = TypeVar("I")
T = TypeVar("T")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
R = TypeVar("R")

@dataclass(init=False)
class Ok(Generic[T]):
  kind: Literal[ResultKind.OK]
  val: T

  def __init__(self, val: T):
    self.kind = ResultKind.OK
    self.val = val

  def map(self, f: Callable[[T], R]) -> "Result[R]":
    return Ok(f(self.val))

  def try_map(self, f: "Callable[[T], Result[R]]") -> "Result[R]":
    return f(self.val)

  def validate(self, msg: str, f: Callable[[T], bool]) -> "Result[T]":
    if not f(self.val):
      return Err(msg)
    return self

  def unwrap(self) -> T:
    return self.val

@dataclass(init=False)
class Err:
  kind: Literal[ResultKind.ERR]
  msg: str

  def __init__(self, msg: str):
    self.kind = ResultKind.ERR
    self.msg = msg

  def map(self, f: Callable[[Any], Any]) -> "Err":
    return self

  def try_map(self, f: Callable[[Any], Any]) -> "Err":
    return self

  def validate(self, msg: str, f: Callable[[Any], bool]) -> "Err":
    return self

  def unwrap(self) -> NoReturn:
    raise ValueError(self.msg)

Result = Ok[T] | Err

@dataclass
class Parser(Generic[I, T]):
  f: Callable[[I], tuple[I, Result[T]]]

  def parse(self, inp: I) -> Result[T]:
    return self.f(inp)[1]

  def map(self, transformer: Callable[[T], R]) -> "Parser[I, R]":
    def map_impl(inp: I) -> "tuple[I, Result[R]]":
      nxt, res = self.f(inp)
      return (nxt, res.map(transformer))
    return Parser(map_impl)

  def try_map(self, transformer: Callable[[T], Result[R]]) -> "Parser[I, R]":
    def try_map_impl(inp: I) -> "tuple[I, Result[R]]":
      nxt, res = self.f(inp)
      res2 = res.try_map(transformer)
      if res2.kind is ResultKind.OK:
        return (nxt, res2)
      return (inp, res2)
    return Parser(try_map_impl)

  def validate(self, msg: str, check: Callable[[T], bool]) -> "Parser[I, T]":
    def validate_impl(inp: I) -> "tuple[I, Result[T]]":
      nxt, res = self.f(inp)
      res2 = res.validate(msg, check)
      if res2.kind is ResultKind.OK:
        return (nxt, res2)
      return (inp, res2)
    return Parser(validate_impl)

  def and_then(self, parser2_func: "Callable[[T], Parser[I, R]]") -> "Parser[I, R]":
    def and_then_impl(inp: I) -> "tuple[I, Result[R]]":
      nxt1, res1 = self.f(inp)
      if res1.kind is ResultKind.ERR:
        return (inp, res1)
      parser2 = parser2_func(res1.val)
      nxt2, res2 = parser2.f(nxt1)
      if res2.kind is ResultKind.ERR:
        return (inp, res2)
      return (nxt2, res2)
    return Parser(and_then_impl)

  def __or__(self, parser2: "Parser[I, T]") -> "Parser[I, T]":
    return alt(self, parser2)

  def __lshift__(self, parser2: "Parser[I, Any]") -> "Parser[I, T]":
    return terminated(self, parser2)

  def __rshift__(self, parser2: "Parser[I, T2]") -> "Parser[I, T2]":
    return preceded(self, parser2)

def consecutive(parser1: Parser[I, T1], parser2: Parser[I, T2], combiner: Callable[[T1, T2], R]) -> Parser[I, R]:
  def consecutive_impl(inp: I) -> tuple[I, Result[R]]:
    nxt1, res1 = parser1.f(inp)
    if res1.kind is ResultKind.ERR:
      return inp, res1
    nxt2, res2 = parser2.f(nxt1)
    if res2.kind is ResultKind.ERR:
      return inp, res2
    return nxt2, res2.map(lambda v2: combiner(res1.val, v2))
  return Parser(consecutive_impl)

def preceded(parser1: Parser[I, Any], parser2: Parser[I, T]) -> Parser[I, T]:
  return consecutive(parser1, parser2, lambda _, v2: v2)

def terminated(parser1: Parser[I, T], parser2: Parser[I, Any]) -> Parser[I, T]:
  return consecutive(parser1, parser2, lambda v1, _: v1)

def optional(parser: Parser[I, T], default: T) -> Parser[I, T]:
  def optional_impl(inp: I) -> tuple[I, Result[T]]:
    nxt1, res1 = parser.f(inp)
    if res1.kind is ResultKind.OK:
      return nxt1, res1
    else:
      return nxt1, Ok(default)
  return Parser(optional_impl)

def alt(*parsers: Parser[I, T]) -> Parser[I, T]:
  def alt_impl(inp: I) -> tuple[I, Result[T]]:
    nxt, res = parsers[0].f(inp)
    for parser in parsers[1:]:
      if res.kind is ResultKind.OK:
        return nxt, res
      nxt, res = parser.f(inp)
    return nxt, res
  return Parser(alt_impl)

def many_m_n(min: int, max: int, parser: Parser[I, T]) -> Parser[I, list[T]]:
  def many_impl(inp: I) -> tuple[I, Result[list[T]]]:
    fullparsed: list[T] = []
    curr = inp
    for _ in range(max):
      curr, res = parser.f(curr)
      if res.kind is ResultKind.ERR:
        if len(fullparsed) < min:
          return inp, Err("many " + res.msg)
        break
      else:
        fullparsed.append(res.val)
    return curr, Ok(fullparsed)
  return Parser(many_impl)

def count(n: int, parser: Parser[I, T]) -> Parser[I, list[T]]:
  return many_m_n(n, n, parser)

def many0(parser: Parser[I, T]) -> Parser[I, list[T]]:
  return many_m_n(0, ~(-1 << 63), parser)

def many1(parser: Parser[I, T]) -> Parser[I, list[T]]:
  return many_m_n(1, ~(-1 << 63), parser)

def many_till_m(min: int, parser: Parser[I, T], end_parser: Parser[I, Any]) -> Parser[I, list[T]]:
  def many_till_impl(inp: I) -> tuple[I, Result[list[T]]]:
    fullparsed: list[T] = []
    curr = inp
    while True:
      post, end_res = end_parser.f(curr)
      if end_res.kind is ResultKind.OK:
        if len(fullparsed) < min:
          return inp, Err("many_till")
        return post, Ok(fullparsed)
      curr, res = parser.f(curr)
      if res.kind is ResultKind.ERR:
        return inp, Err("many_till " + res.msg)
      else:
        fullparsed.append(res.val)
  return Parser(many_till_impl)

def many_till(parser: Parser[I, T], end_parser: Parser[I, Any]) -> Parser[I, list[T]]:
  return many_till_m(0, parser, end_parser)

def many_till1(parser: Parser[I, T], end_parser: Parser[I, Any]) -> Parser[I, list[T]]:
  return many_till_m(1, parser, end_parser)

def fold_many_m(min: int, parser: Parser[I, T], folder: Callable[[R, T], R], initial: R) -> Parser[I, R]:
  def fold_many_impl(inp: I) -> tuple[I, Result[R]]:
    acc = initial
    curr = inp
    n_matched = 0
    while True:
      curr, res = parser.f(curr)
      if res.kind is ResultKind.ERR:
        if n_matched < min:
          return inp, Err("fold_many " + res.msg)
        return curr, Ok(acc)
      else:
        n_matched += 1
        acc = folder(acc, res.val)
  return Parser(fold_many_impl)

def fold_many0(parser: Parser[I, T], folder: Callable[[R, T], R], initial: R) -> Parser[I, R]:
  return fold_many_m(0, parser, folder, initial)

def fold_many1(parser: Parser[I, T], folder: Callable[[R, T], R], initial: R) -> Parser[I, R]:
  return fold_many_m(1, parser, folder, initial)

def fold_many_till_m(min: int, parser: Parser[I, T], end_parser: Parser[I, Any], folder: Callable[[R, T], R], initial: R) -> Parser[I, R]:
  def fold_many_till_impl(inp: I) -> tuple[I, Result[R]]:
    acc = initial
    curr = inp
    n_matched = 0
    while True:
      post, end_res = end_parser.f(curr)
      if end_res.kind is ResultKind.OK:
        if n_matched < min:
          return inp, Err("many_till")
        return post, Ok(acc)
      curr, res = parser.f(curr)
      if res.kind is ResultKind.ERR:
        return inp, Err("fold_many_till " + res.msg)
      else:
        n_matched += 1
        acc = folder(acc, res.val)
  return Parser(fold_many_till_impl)

def fold_many_till(parser: Parser[I, T], end_parser: Parser[I, Any], folder: Callable[[R, T], R], initial: R) -> Parser[I, R]:
  return fold_many_till_m(0, parser, end_parser, folder, initial)

def fold_many_till1(parser: Parser[I, T], end_parser: Parser[I, Any], folder: Callable[[R, T], R], initial: R) -> Parser[I, R]:
  return fold_many_till_m(1, parser, end_parser, folder, initial)

def succeed(v: T) -> Parser[Any, T]:
  return Parser(lambda inp: (inp, Ok(v)))

def fail(msg: str) -> Parser[Any, Any]:
  return Parser(lambda inp: (inp, Err(msg)))

def any_element() -> Parser[Sequence[E], E]:
  def any_element_impl(inp: Sequence[E]) -> tuple[Sequence[E], Result[E]]:
    if len(inp) == 0:
      return inp, Err("any_element")
    return inp[1:], Ok(inp[0])
  return Parser(any_element_impl)

def peek_cond(*parsers: tuple[Callable[[E], bool], Parser[Sequence[E], R]]) -> Parser[Sequence[E], R]:
  def peek_cond_impl(inp: Sequence[E]) -> tuple[Sequence[E], Result[R]]:
    if len(inp) == 0:
      return inp, Err("peek_cond")
    for predicate, parser in parsers:
      if predicate(inp[0]):
        return parser.f(inp)
    return inp, Err("peek_cond")
  return Parser(peek_cond_impl)

def satisfy(check: Callable[[E], bool]) -> Parser[Sequence[E], E]:
  def satisfy_impl(inp: Sequence[E]) -> tuple[Sequence[E], Result[E]]:
    if len(inp) == 0 or not check(inp[0]):
      return inp, Err("satisfy")
    return inp[1:], Ok(inp[0])
  return Parser(satisfy_impl)

def satisfy_eq(item: E) -> Parser[Sequence[E], E]:
  def satisfy_eq_impl(inp: Sequence[E]) -> tuple[Sequence[E], Result[E]]:
    if len(inp) == 0 or inp[0] != item:
      return inp, Err("satisfy_eq")
    return inp[1:], Ok(inp[0])
  return Parser(satisfy_eq_impl)

def satisfy_in(items: Container[E]) -> Parser[Sequence[E], E]:
  def satisfy_in_impl(inp: Sequence[E]) -> tuple[Sequence[E], Result[E]]:
    if len(inp) == 0 or inp[0] not in items:
      return inp, Err("satisfy_in")
    return inp[1:], Ok(inp[0])
  return Parser(satisfy_in_impl)

def satisfy_subseq(subseq: Sequence[E]) -> Parser[Sequence[E], Sequence[E]]:
  def satisfy_subseq_impl(inp: Sequence[E]) -> tuple[Sequence[E], Result[Sequence[E]]]:
    sz = len(subseq)
    if len(inp) < sz or inp[:sz] != subseq:
      return inp, Err("satisfy_subseq")
    return inp[sz:], Ok(inp[:sz])
  return Parser(satisfy_subseq_impl)

def take_while_m(min: int, predicate: Callable[[str], bool], complement: bool = False) -> Parser[str, str]:
  def take_while_impl(inp: str) -> tuple[str, Result[str]]:
    if len(inp) < min:
      return inp, Err("take_while")
    for index in range(len(inp)):
      if predicate(inp[index]) is complement:
        if index < min:
          return inp[index:], Err("take_while")
        return inp[index:], Ok(inp[:index])
    return inp[len(inp):], Ok(inp)
  return Parser(take_while_impl)

def take_while(predicate: Callable[[str], bool]) -> Parser[str, str]:
  return take_while_m(0, predicate)

def take_while1(predicate: Callable[[str], bool]) -> Parser[str, str]:
  return take_while_m(1, predicate)

def take_till(predicate: Callable[[str], bool]) -> Parser[str, str]:
  return take_while_m(0, predicate, True)

def take_till1(predicate: Callable[[str], bool]) -> Parser[str, str]:
  return take_while_m(1, predicate, True)

def take_seq_while_m(min: int, predicate: Callable[[E], bool], complement: bool = False) -> Parser[Sequence[E], list[E]]:
  def take_seq_while_impl(inp: Sequence[E]) -> tuple[Sequence[E], Result[list[E]]]:
    if len(inp) < min:
      return inp, Err("take_seq_while")
    for index in range(len(inp)):
      if predicate(inp[index]) is complement:
        if index < min:
          return inp[index:], Err("take_seq_while")
        return inp[index:], Ok(list(inp[:index]))
    return inp[len(inp):], Ok(list(inp))
  return Parser(take_seq_while_impl)

def take_seq_while(predicate: Callable[[E], bool]) -> Parser[Sequence[E], list[E]]:
  return take_seq_while_m(0, predicate)

def take_seq_while1(predicate: Callable[[E], bool]) -> Parser[Sequence[E], list[E]]:
  return take_seq_while_m(1, predicate)

def take_seq_till(predicate: Callable[[E], bool]) -> Parser[Sequence[E], list[E]]:
  return take_seq_while_m(0, predicate, True)

def take_seq_till1(predicate: Callable[[E], bool]) -> Parser[Sequence[E], list[E]]:
  return take_seq_while_m(1, predicate, True)

def validate_remaining(check: Callable[[I], bool]) -> Parser[I, bool]:
  def validate_remaining_impl(inp: I) -> tuple[I, Result[bool]]:
    if not check(inp):
      return inp, Err("validate_remaining")
    else:
      return inp, Ok(True)
  return Parser(validate_remaining_impl)

def all_consuming(parser: Parser[Sequence[E], T]) -> Parser[Sequence[E], T]:
  def is_empty(rest: Sequence[E]) -> bool:
    return len(rest) == 0
  return terminated(parser, validate_remaining(is_empty))

@dataclass
class Lift2(Generic[T1, T2, R]):
  f: Callable[[T1, T2], R]

  def lift(self, lowered1: Callable[[I], T1], lowered2: Callable[[I], T2]) -> Callable[[I], R]:
    return lambda inp: self.f(lowered1(inp), lowered2(inp))
