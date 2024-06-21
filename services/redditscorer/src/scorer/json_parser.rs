// based on https://github.com/rust-bakery/parser_benchmarks/blob/master/json/nom/src/main.rs
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_while, take_while1},
    character::complete::{anychar, char, one_of},
    combinator::{all_consuming, cut, map, map_opt, opt, value},
    error::ParseError,
    multi::{fold_many0, fold_many_m_n, separated_list0},
    number::complete::double,
    sequence::{preceded, terminated, tuple},
    IResult, Needed, Offset, Parser,
};

use std::borrow::{Borrow, Cow};

pub fn is_string_character(c: u8) -> bool {
    c != b'"' && c != b'\\'
}

pub fn is_space(c: u8) -> bool {
    c == b' ' || c == b'\t' || c == b'\r' || c == b'\n'
}

fn sp<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&[u8], &[u8], E> {
    take_while(is_space)(i)
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

fn skip_string<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], (), E> {
    preceded(
        char('\"'),
        cut(value(
            (),
            alt((
                char('\"'),
                preceded(
                    escaped(take_while1(is_string_character), '\\', anychar),
                    char('\"'),
                ),
            )),
        )),
    )(i)
}

fn null<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], (), E> {
    value((), tag("null"))(input)
}

fn boolean<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&[u8], bool, E> {
    alt((map(tag("false"), |_| false), map(tag("true"), |_| true)))(i)
}

fn skip_array<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], (), E> {
    preceded(
        char('['),
        cut(terminated(
            value(
                (),
                separated_list0(preceded(sp, char(',')), skip_json_value),
            ),
            preceded(sp, char(']')),
        )),
    )(i)
}

#[derive(Clone)]
enum KeyValue<'a> {
    Url(Option<Cow<'a, str>>),
    Title(Option<Cow<'a, str>>),
    None,
}

fn skip_key_value<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], (), E> {
    let (i1, key) = preceded(sp, string)(i)?;
    let (i2, _) = cut(preceded(sp, char(':')))(i1)?;
    skip_json_value(i2)
}

fn key_value<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], KeyValue<'a>, E> {
    let (i1, key) = preceded(sp, string)(i)?;
    let (i2, _) = cut(preceded(sp, char(':')))(i1)?;
    let (i3, kv) = match key.borrow() {
        "url" => {
            let (i3, v) = json_value_keep_if_string(i2)?;
            (i3, KeyValue::Url(v))
        }
        "title" => {
            let (i3, v) = json_value_keep_if_string(i2)?;
            (i3, KeyValue::Title(v))
        }
        _ => value(KeyValue::None, skip_json_value)(i2)?,
    };
    Ok((i3, kv))
}

fn skip_hash<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], (), E> {
    let (i, _) = char('{')(i)?;

    match skip_key_value::<E>(i) {
        Err(_) => value((), preceded(sp, char('}')))(i),
        Ok((i, first)) => {
            let (i1, _) = fold_many0(
                preceded(preceded(sp, char(',')), skip_key_value),
                || (),
                |_, _| (),
            )(i)?;
            value((), preceded(sp, char('}')))(i1)
        }
    }
}

fn hash<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], SubmissionMeta<'a>, E> {
    let (i, _) = char('{')(i)?;

    match skip_key_value::<E>(i) {
        Err(_) => value(SubmissionMeta::default(), preceded(sp, char('}')))(i),
        Ok((i, first)) => {
            let (i1, v) = fold_many0(
                preceded(preceded(sp, char(',')), key_value),
                SubmissionMeta::default,
                |prev, curr| match curr {
                    KeyValue::Url(url) => SubmissionMeta {
                        url,
                        title: prev.title,
                    },
                    KeyValue::Title(title) => SubmissionMeta {
                        url: prev.url,
                        title,
                    },
                    KeyValue::None => prev,
                },
            )(i)?;
            let (i2, _) = preceded(sp, char('}'))(i1)?;
            Ok((i2, v))
        }
    }
}

fn skip_json_value<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], (), E> {
    let (rest, _) = sp(i)?;
    match rest.get(0) {
        Some(byte) => match byte {
            b'{' => skip_hash(rest),
            b'[' => skip_array(rest),
            b'"' => skip_string(rest),
            b't' | b'f' => value((), boolean)(rest),
            b'n' => value((), null)(rest),
            b'-' | b'0'..=b'9' => value((), double)(rest),
            _ => Err(nom::Err::Error(E::from_error_kind(
                rest,
                nom::error::ErrorKind::Char,
            ))),
        },
        None => Err(nom::Err::Incomplete(Needed::new(1))),
    }
}

fn json_value_keep_if_hash<'a, E: ParseError<&'a [u8]>>(
    i: &'a [u8],
) -> IResult<&'a [u8], SubmissionMeta<'a>, E> {
    let (rest, _) = sp(i)?;
    match rest.get(0) {
        Some(byte) => match byte {
            b'{' => hash(rest),
            b'[' => value(SubmissionMeta::default(), skip_array)(rest),
            b'"' => value(SubmissionMeta::default(), skip_string)(rest),
            b't' | b'f' => value(SubmissionMeta::default(), boolean)(rest),
            b'n' => value(SubmissionMeta::default(), null)(rest),
            b'-' | b'0'..=b'9' => value(SubmissionMeta::default(), double)(rest),
            _ => Err(nom::Err::Error(E::from_error_kind(
                rest,
                nom::error::ErrorKind::Char,
            ))),
        },
        None => Err(nom::Err::Incomplete(Needed::new(1))),
    }
}

fn json_value_keep_if_string<'a, E: ParseError<&'a [u8]>>(
    i: &'a [u8],
) -> IResult<&'a [u8], Option<Cow<'a, str>>, E> {
    let (rest, _) = sp(i)?;
    match rest.get(0) {
        Some(byte) => match byte {
            b'{' => value(None, skip_hash)(rest),
            b'[' => value(None, skip_array)(rest),
            b'"' => map(string, Some)(rest),
            b't' | b'f' => value(None, boolean)(rest),
            b'n' => value(None, null)(rest),
            b'-' | b'0'..=b'9' => value(None, double)(rest),
            _ => Err(nom::Err::Error(E::from_error_kind(
                rest,
                nom::error::ErrorKind::Char,
            ))),
        },
        None => Err(nom::Err::Incomplete(Needed::new(1))),
    }
}

#[derive(Default, Debug, Clone)]
pub(crate) struct SubmissionMeta<'a> {
    pub url: Option<Cow<'a, str>>,
    pub title: Option<Cow<'a, str>>,
}

pub(crate) fn parse_json<'a, 'b, E: ParseError<&'a [u8]>>(
    input: &'a [u8],
) -> Result<SubmissionMeta<'a>, nom::Err<E>> {
    let (trailing, value) = json_value_keep_if_hash(input)?;
    let _ = all_consuming(sp)(trailing)?;
    Ok(value)
}
