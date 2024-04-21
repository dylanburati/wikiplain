// based on https://github.com/rust-bakery/parser_benchmarks/blob/master/json/nom/src/main.rs
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{anychar, char, one_of},
    combinator::{all_consuming, cut, map, map_opt, opt, value},
    error::ParseError,
    multi::fold_many_m_n,
    number::complete::double,
    sequence::{preceded, terminated, tuple},
    IResult, Needed, Offset, Parser,
};

use std::borrow::Cow;
use std::ops::ControlFlow;

pub fn is_string_character(c: u8) -> bool {
    c != b'"' && c != b'\\'
}

pub fn is_space(c: u8) -> bool {
    c == b' ' || c == b'\t' || c == b'\r' || c == b'\n'
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JsonPathItem<'a> {
    Array,
    Key(&'a str),
}

#[repr(transparent)]
#[derive(Clone)]
pub struct SelectorSlice<'a>(&'a [JsonPathItem<'a>]);

impl SelectorSlice<'_> {
    pub fn strip_first(&self, prefix: &JsonPathItem) -> Option<Self> {
        match self.0.split_first() {
            Some((first, rest)) if first == prefix => Some(Self(rest)),
            _ => None,
        }
    }
}

impl<'a> From<&'a Selector<'a>> for SelectorSlice<'a> {
    fn from(value: &'a Selector<'a>) -> Self {
        SelectorSlice(&value.0)
    }
}

pub struct Selector<'a>(Vec<JsonPathItem<'a>>);

impl<'a> Selector<'a> {
    pub fn new(items: Vec<JsonPathItem<'a>>) -> Self {
        Self(items)
    }
}

fn sp<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&[u8], &[u8], E> {
    take_while(is_space)(i)
}

#[derive(Debug, Clone, PartialEq)]
pub enum JsonValue<'a> {
    Null,
    Str(Cow<'a, str>),
    Boolean(bool),
    Num(f64),
    Array(Vec<JsonValue<'a>>),
    Object(Vec<(Cow<'a, str>, JsonValue<'a>)>),
}

fn string<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], Cow<'a, str>, E> {
    let (i1, _) = char('\"')(i)?;
    let (i2, s) = cut(string_interior)(i1)?;
    let (i3, _) = char('"')(i2)?;
    Ok((i3, s))
}

fn string_interior<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], Cow<'a, str>, E> {
    let (mut i1, plain) = take_while(is_string_character)(i)?;
    if let (_, Some(_)) = opt(char('"'))(i1)? {
        let Ok(v) = std::str::from_utf8(plain) else {
            return Err(nom::Err::Error(E::from_error_kind(
                i,
                nom::error::ErrorKind::Fail,
            )));
        };
        return Ok((i1, Cow::Borrowed(v)));
    };
    // input:           "beginning\u1234\nmiddle\end"
    //  i always points ^         |     |       |   |
    // first escape:              ^i1   ^i2     |   |
    // loop1:                           ^i1     ^i2 |
    // loop2:                                   ^i1 ^i2
    // outside:                                     ^i1
    let mut acc = Vec::with_capacity(plain.len() + 4);
    acc.extend_from_slice(plain);
    match i1.get(0) {
        Some(b'\\') => {
            let (i2, chr) = alt((uniescape, hex_escape, octal_escape, anychar))(&i1[1..])?;
            let l = acc.len();
            let chrlen = chr.len_utf8();
            acc.resize(l + chrlen, 0);
            let _ = chr.encode_utf8(&mut acc[l..]);
            i1 = i2;
        }
        _ => {
            return Err(nom::Err::Error(E::from_error_kind(
                i,
                nom::error::ErrorKind::Char,
            )));
        }
    };
    let mut normal = take_while1(is_string_character);
    loop {
        match normal.parse(i1) {
            Ok((i2, _)) => {
                if i2.len() == 0 {
                    i1 = i2;
                    break;
                } else if i2.len() == i1.len() {
                    return Err(nom::Err::Error(E::from_error_kind(
                        i2,
                        nom::error::ErrorKind::EscapedTransform,
                    )));
                } else {
                    acc.extend_from_slice(&i1[..i1.offset(&i2)]);
                    i1 = i2;
                }
            }
            Err(nom::Err::Error(_)) => match i1.get(0) {
                Some(b'\\') => {
                    let (i2, chr) = alt((uniescape, hex_escape, octal_escape, anychar))(&i1[1..])?;
                    let l = acc.len();
                    let chrlen = chr.len_utf8();
                    acc.resize(l + chrlen, 0);
                    let _ = chr.encode_utf8(&mut acc[l..]);
                    i1 = i2;
                }
                Some(b'"') => break,
                _ => {
                    return Err(nom::Err::Error(E::from_error_kind(
                        i,
                        nom::error::ErrorKind::Char,
                    )));
                }
            },
            Err(other) => return Err(other),
        }
    }
    let Ok(v) = String::from_utf8(acc) else {
        return Err(nom::Err::Error(E::from_error_kind(
            i,
            nom::error::ErrorKind::Fail,
        )));
    };
    Ok((i1, Cow::Owned(v)))
}

fn uniescape<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], char, E> {
    preceded(
        char('u'),
        map_opt(
            tuple((
                one_of("0123456789abcdefABCDEF"),
                one_of("0123456789abcdefABCDEF"),
                one_of("0123456789abcdefABCDEF"),
                one_of("0123456789abcdefABCDEF"),
            )),
            |(d3, d2, d1, d0)| {
                let code = (d3.to_digit(16).unwrap() << 12)
                    | (d2.to_digit(16).unwrap() << 8)
                    | (d1.to_digit(16).unwrap() << 4)
                    | d0.to_digit(16).unwrap();
                char::from_u32(code)
            },
        ),
    )(i)
}

fn hex_escape<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], char, E> {
    terminated(
        char('x'),
        map(
            tuple((
                one_of("0123456789abcdefABCDEF"),
                one_of("0123456789abcdefABCDEF"),
            )),
            |(d1, d0)| {
                let code = (d1.to_digit(16).unwrap() << 4) | d0.to_digit(16).unwrap();
                char::from_u32(code).unwrap()
            },
        ),
    )(i)
}

fn octal_escape<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], char, E> {
    let (i1, d) = one_of("01234567")(i)?;
    let (i2, code) = if d <= '3' {
        fold_many_m_n(
            0,
            2,
            one_of("01234567"),
            || d.to_digit(8).unwrap(),
            |n, di| (n << 3) | di.to_digit(8).unwrap(),
        )(i1)?
    } else {
        fold_many_m_n(
            0,
            1,
            one_of("01234567"),
            || d.to_digit(8).unwrap(),
            |n, di| (n << 3) | di.to_digit(8).unwrap(),
        )(i1)?
    };
    Ok((i2, char::from_u32(code).unwrap()))
}

fn null<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], (), E> {
    value((), tag("null"))(input)
}

fn boolean<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&[u8], bool, E> {
    alt((map(tag("false"), |_| false), map(tag("true"), |_| true)))(i)
}

fn array<'a, E: ParseError<&'a [u8]>>(
    i: &'a [u8],
    selectors: Vec<SelectorSlice<'a>>,
) -> IResult<&'a [u8], JsonValue<'a>, E> {
    let (i, _) = char('[')(i)?;
    if let (post, Some(_)) = opt(preceded(sp, char(']')))(i)? {
        let a = if selectors.is_empty() {
            JsonValue::Null
        } else {
            JsonValue::Array(vec![])
        };
        return Ok((post, a));
    };

    let (i1, first_elem) = json_value(i, selectors.clone())?;
    let (mut i2, _) = sp(i1)?;
    let mut delim_parser = alt((
        map(char(','), |_| ControlFlow::Continue(())),
        map(char(']'), |_| ControlFlow::Break(())),
    ));
    if selectors.is_empty() {
        loop {
            match delim_parser.parse(i2)? {
                (ielem, ControlFlow::Continue(_)) => {
                    // discard
                    let (i3, _) = json_value(ielem, selectors.clone())?;
                    let (i4, _) = sp(i3)?;
                    i2 = i4;
                }
                (post, ControlFlow::Break(_)) => return Ok((post, JsonValue::Null)),
            }
        }
    }

    let mut elems = vec![first_elem];
    loop {
        match delim_parser.parse(i2)? {
            (ielem, ControlFlow::Continue(_)) => {
                let (i3, curr_elem) = json_value(ielem, selectors.clone())?;
                let (i4, _) = sp(i3)?;
                elems.push(curr_elem);
                i2 = i4;
            }
            (post, ControlFlow::Break(_)) => return Ok((post, JsonValue::Array(elems))),
        }
    }
}

fn key_value<'a, E: ParseError<&'a [u8]>>(
    i: &'a [u8],
    selectors: Vec<SelectorSlice<'a>>,
) -> IResult<&'a [u8], (Cow<'a, str>, JsonValue<'a>), E> {
    let (i1, key) = preceded(sp, string)(i)?;
    let (i2, _) = cut(preceded(sp, char(':')))(i1)?;
    let prefix = JsonPathItem::Key(&key);
    let inner_selectors = selectors
        .into_iter()
        .filter_map(|sel| sel.strip_first(&prefix))
        .collect();
    let (i3, value) = json_value(i2, inner_selectors)?;
    Ok((i3, (key, value)))
}

fn hash<'a, E: ParseError<&'a [u8]>>(
    i: &'a [u8],
    selectors: Vec<SelectorSlice<'a>>,
) -> IResult<&'a [u8], JsonValue<'a>, E> {
    let (i, _) = char('{')(i)?;
    if let (post, Some(_)) = opt(preceded(sp, char('}')))(i)? {
        let h = if selectors.is_empty() {
            JsonValue::Null
        } else {
            JsonValue::Object(vec![])
        };
        return Ok((post, h));
    };

    let (i1, first_kv) = key_value(i, selectors.clone())?;
    let (mut i2, _) = sp(i1)?;
    let mut delim_parser = alt((
        map(char(','), |_| ControlFlow::Continue(())),
        map(char('}'), |_| ControlFlow::Break(())),
    ));
    if selectors.is_empty() {
        loop {
            match delim_parser.parse(i2)? {
                (ikv, ControlFlow::Continue(_)) => {
                    // discard
                    let (i3, _) = key_value(ikv, selectors.clone())?;
                    let (i4, _) = sp(i3)?;
                    i2 = i4;
                }
                (post, ControlFlow::Break(_)) => return Ok((post, JsonValue::Null)),
            }
        }
    }

    let mut kvs = vec![];
    let keep_all = selectors.iter().any(|sel| sel.0.is_empty()); // fully matched selector slice
    if keep_all
        || selectors
            .iter()
            .any(|sel| matches!(sel.0.first(), Some(JsonPathItem::Key(k)) if &first_kv.0 == k))
    {
        kvs.push(first_kv);
    }
    loop {
        match delim_parser.parse(i2)? {
            (ikv, ControlFlow::Continue(_)) => {
                // keep
                let (i3, curr_kv) = key_value(ikv, selectors.clone())?;
                let (i4, _) = sp(i3)?;
                if keep_all
                    || selectors.iter().any(
                        |sel| matches!(sel.0.first(), Some(JsonPathItem::Key(k)) if &curr_kv.0 == k),
                    )
                {
                    kvs.push(curr_kv);
                }
                i2 = i4;
            }
            (post, ControlFlow::Break(_)) => return Ok((post, JsonValue::Object(kvs))),
        }
    }
}

fn json_value<'a, E: ParseError<&'a [u8]>>(
    i: &'a [u8],
    selectors: Vec<SelectorSlice<'a>>,
) -> IResult<&'a [u8], JsonValue<'a>, E> {
    let (rest, _) = sp(i)?;
    match rest.get(0) {
        Some(byte) => match byte {
            b'{' => hash(rest, selectors),
            b'[' => {
                let prefix = JsonPathItem::Array;
                let inner_selectors = selectors
                    .into_iter()
                    .filter_map(|sel| sel.strip_first(&prefix))
                    .collect();
                array(rest, inner_selectors)
            }
            b'"' => map(string, JsonValue::Str)(rest),
            b't' | b'f' => map(boolean, JsonValue::Boolean)(rest),
            b'n' => value(JsonValue::Null, null)(rest),
            b'-' | b'0'..=b'9' => map(double, JsonValue::Num)(rest),
            _ => Err(nom::Err::Error(E::from_error_kind(
                rest,
                nom::error::ErrorKind::Char,
            ))),
        },
        None => Err(nom::Err::Incomplete(Needed::new(1))),
    }
}

pub(crate) fn parse_json<'a, E: ParseError<&'a [u8]>>(
    input: &'a [u8],
    selectors: &'a [Selector<'a>],
) -> IResult<&'a [u8], JsonValue<'a>, E> {
    let selector_slices = selectors.iter().map(|e| e.into()).collect();
    let (trailing, value) = json_value(input, selector_slices)?;
    let (empty, _) = all_consuming(sp)(trailing)?;
    Ok((empty, value))
}
