// based on https://github.com/rust-bakery/parser_benchmarks/blob/master/json/nom/src/main.rs
use nom::{
    branch::alt,
    bytes::complete::{escaped, is_a, tag, take_while, take_while1, take_while_m_n},
    character::complete::{alphanumeric1 as alphanumeric, anychar, char, one_of, satisfy},
    combinator::{cut, iterator, map, map_res, opt, value},
    error::{ErrorKind, ParseError, VerboseError},
    multi::separated_list0,
    number::complete::double,
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Compare, CompareResult, Err, FindSubstring, IResult, InputIter, InputLength, InputTake,
    InputTakeAtPosition, Needed, Offset, Slice,
};

use std::{
    collections::HashMap,
    io::{stdin, BufRead, BufReader},
    iter::{Copied, Enumerate},
    ops::{Deref, Range, RangeFrom, RangeFull, RangeTo},
    rc::Rc,
    slice::Iter,
    str,
};

pub fn is_string_character(c: u8) -> bool {
    //FIXME: should validate unicode character
    c != b'"' && c != b'\\'
}

pub fn is_space(c: u8) -> bool {
    c == b' ' || c == b'\t' || c == b'\r' || c == b'\n'
}

enum JsonPathItem<'a> {
    Array,
    Key(&'a str),
}

#[derive(Debug, Clone)]
struct ParsePath<'a> {
    prev: Option<Rc<ParsePath<'a>>>,
    item: JsonPathItem<'a>,
}

#[derive(Debug, Clone)]
struct ParseState<'a> {
    input: &'a [u8],
    path: Option<Rc<ParsePath<'a>>>,
}

impl<'a> ParseState<'a> {
    #[inline]
    pub(crate) fn of(&self, i: &'a [u8]) -> Self {
        Self {
            input: i,
            path: self.path,
        }
    }

    pub(crate) fn push_path(&self, item: JsonPathItem<'a>) -> Self {
        Self {
            input: self.input,
            path: Some(Rc::new(ParsePath {
                prev: self.path.as_ref().map(Rc::clone),
                item,
            }))
        }
    }

    pub(crate) fn pop_path(&self) -> Option<(Self, &JsonPathItem<'a>)> {
        let (path, item) = match &self.path {
            Some(ParsePath { prev, item }) => (prev.as_ref().map(Rc::clone), item),
            None => return None,
        };

        let state = Self {
            input: self.input,
            path,
        };

        Some((state, item))
    }
}

impl<'a> Deref for ParseState<'a> {
    type Target = [u8];

    #[inline]
    fn deref(&self) -> &'a [u8] {
        self.input
    }
}

impl<'a> Slice<Range<usize>> for ParseState<'a> {
    fn slice(&self, range: Range<usize>) -> Self {
        self.of(self.input.slice(range))
    }
}

impl<'a> Slice<RangeTo<usize>> for ParseState<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.of(self.input.slice(range))
    }
}

impl<'a> Slice<RangeFrom<usize>> for ParseState<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.of(self.input.slice(range))
    }
}

impl<'a> Slice<RangeFull> for ParseState<'a> {
    fn slice(&self, range: RangeFull) -> Self {
        self.of(self.input.slice(range))
    }
}

impl<'a, 'b> FindSubstring<&'b str> for ParseState<'a> {
    fn find_substring(&self, substr: &str) -> Option<usize> {
        self.input.find(substr)
    }
}

impl<'a, 'b> Compare<&'b str> for ParseState<'a> {
    #[inline]
    fn compare(&self, t: &'b str) -> CompareResult {
        self.input.compare(t)
    }

    #[inline]
    fn compare_no_case(&self, t: &'b str) -> CompareResult {
        self.input.compare_no_case(t)
    }
}

impl<'a> InputLength for ParseState<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.len()
    }
}

impl<'a> InputIter for ParseState<'a> {
    type Item = u8;
    type Iter = Enumerate<Self::IterElem>;
    type IterElem = Copied<Iter<'a, u8>>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.iter_elements().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.iter().copied()
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.iter().position(|b| predicate(*b))
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.len() >= count {
            Ok(count)
        } else {
            Err(Needed::new(count - self.len()))
        }
    }
}

impl<'a> InputTake for ParseState<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        let s = self.input.take(count);
        self.of(s)
    }
    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (l, r) = self.input.take_split(count);
        (self.of(l), self.of(r))
    }
}

impl<'a> InputTakeAtPosition for ParseState<'a> {
    type Item = char;

    #[inline]
    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.input.split_at_position::<_, (&str, ErrorKind)>(predicate) {
            Ok((l, r)) => Ok((self.of(l), self.of(r))),
            Err(Err::Error((i, kind))) => Err(Err::Error(E::from_error_kind(self.of(i), kind))),
            Err(Err::Failure((i, kind))) => Err(Err::Failure(E::from_error_kind(self.of(i), kind))),
            Err(Err::Incomplete(x)) => Err(Err::Incomplete(x)),
        }
    }

    #[inline]
    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self
            .input
            .split_at_position1::<_, (&str, ErrorKind)>(predicate, e)
        {
            Ok((l, r)) => Ok((self.of(l), self.of(r))),
            Err(Err::Error((i, kind))) => Err(Err::Error(E::from_error_kind(self.of(i), kind))),
            Err(Err::Failure((i, kind))) => Err(Err::Failure(E::from_error_kind(self.of(i), kind))),
            Err(Err::Incomplete(x)) => Err(Err::Incomplete(x)),
        }
    }

    #[inline]
    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self
            .input
            .split_at_position_complete::<_, (&str, ErrorKind)>(predicate)
        {
            Ok((l, r)) => Ok((self.of(l), self.of(r))),
            Err(Err::Error((i, kind))) => Err(Err::Error(E::from_error_kind(self.of(i), kind))),
            Err(Err::Failure((i, kind))) => Err(Err::Failure(E::from_error_kind(self.of(i), kind))),
            Err(Err::Incomplete(x)) => Err(Err::Incomplete(x)),
        }
    }

    #[inline]
    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self
            .input
            .split_at_position1_complete::<_, (&str, ErrorKind)>(predicate, e)
        {
            Ok((l, r)) => Ok((self.of(l), self.of(r))),
            Err(Err::Error((i, kind))) => Err(Err::Error(E::from_error_kind(self.of(i), kind))),
            Err(Err::Failure((i, kind))) => Err(Err::Failure(E::from_error_kind(self.of(i), kind))),
            Err(Err::Incomplete(x)) => Err(Err::Incomplete(x)),
        }
    }
}

impl<'a> Offset for ParseState<'a> {
    fn offset(&self, second: &Self) -> usize {
        self.input.offset(second.input)
    }
}

fn sp<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&[u8], &[u8], E> {
    take_while(is_space)(i)
}

#[derive(Debug, PartialEq)]
pub enum JsonValue<'a> {
    Null,
    Str(&'a str),
    Boolean(bool),
    Num(f64),
    Array(Vec<JsonValue<'a>>),
    Object(Vec<(&'a str, JsonValue<'a>)>),
}

//FIXME: handle the cases like \u1234
fn string<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&[u8], &str, E> {
    preceded(
        char('\"'),
        cut(terminated(
            map(
                escaped(
                    take_while1(is_string_character),
                    '\\',
                    alt((uniescape, hex_escape, octal_escape, anychar)),
                ),
                |bytes| str::from_utf8(bytes).unwrap(),
            ),
            char('\"'),
        )),
    )(i)
}

fn null<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], (), E> {
    value((), tag("null"))(input)
}

fn boolean<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&[u8], bool, E> {
    alt((map(tag("false"), |_| false), map(tag("true"), |_| true)))(i)
}

fn array<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&[u8], Vec<JsonValue>, E> {
    preceded(
        char('['),
        cut(terminated(
            separated_list0(preceded(sp, char(',')), json_value),
            preceded(sp, char(']')),
        )),
    )(i)
}

fn key_value<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&[u8], (&str, JsonValue), E> {
    separated_pair(
        preceded(sp, string),
        cut(preceded(sp, char(':'))),
        json_value,
    )(i)
}

fn hash<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&[u8], Vec<(&str, JsonValue)>, E> {
    preceded(
        char('{'),
        cut(terminated(
            separated_list0(preceded(sp, char(',')), key_value),
            preceded(sp, char('}')),
        )),
    )(i)
}

fn json_value<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&[u8], JsonValue, E> {
    preceded(
        sp,
        alt((
            map(hash, JsonValue::Object),
            map(array, JsonValue::Array),
            map(string, JsonValue::Str),
            map(double, JsonValue::Num),
            map(boolean, JsonValue::Boolean),
            map(null, |_| JsonValue::Null),
        )),
    )(i)
}

fn root<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&[u8], JsonValue, E> {
    delimited(
        sp,
        alt((map(hash, JsonValue::Object), map(array, JsonValue::Array))),
        opt(sp),
    )(i)
}

fn uniescape<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], char, E> {
    terminated(
        char('u'),
        tuple((
            one_of("0123456789abcdefABCDEF"),
            one_of("0123456789abcdefABCDEF"),
            one_of("0123456789abcdefABCDEF"),
            one_of("0123456789abcdefABCDEF"),
        )),
    )(i)
}

fn hex_escape<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], char, E> {
    terminated(
        char('x'),
        tuple((
            one_of("0123456789abcdefABCDEF"),
            one_of("0123456789abcdefABCDEF"),
        )),
    )(i)
}

fn octal_escape<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], char, E> {
    let (i1, c1) = one_of("01234567")(i)?;
    if c1 <= '3' {
        let (i2, _) = take_while_m_n(0, 2, |c| c >= b'0' && c <= b'7')(i1)?;
        Ok((i2, c1))
    } else {
        let (i2, _) = take_while_m_n(0, 1, |c| c >= b'0' && c <= b'7')(i1)?;
        Ok((i2, c1))
    }
}

fn main() {
    let inp = BufReader::new(stdin());
    let mut unique = HashMap::new();
    for line_res in inp.lines() {
        let line = line_res.unwrap();
        let json = match root::<VerboseError<&[u8]>>(line.as_bytes()) {
            Ok((_, d)) => d,
            Err(e) => {
                panic!(
                    "{:?}",
                    e.map(|inner| inner
                        .errors
                        .into_iter()
                        .map(|(i, k)| (str::from_utf8(i).unwrap_or_else(|_| "<non-utf8>"), k))
                        .collect::<Vec<_>>())
                )
            }
        };
        let mut stack = vec![(json, false)];
        while let Some((el, should_print)) = stack.pop() {
            match el {
                JsonValue::Null | JsonValue::Boolean(_) | JsonValue::Num(_) => {}
                JsonValue::Str(s) => {
                    if should_print {
                        match unique.entry(s.to_owned()) {
                            std::collections::hash_map::Entry::Occupied(_) => {}
                            e @ std::collections::hash_map::Entry::Vacant(_) => {
                                println!("{}", e.key());
                                e.or_insert(());
                            }
                        }
                    }
                }
                JsonValue::Array(a) => {
                    stack.extend(a.into_iter().rev().map(|e| (e, false)));
                }
                JsonValue::Object(obj) => {
                    stack.extend(obj.into_iter().rev().map(|(k, v)| (v, k == "globe")));
                }
            }
        }
    }
}
