use std::{
    cmp::{Eq, PartialEq},
    fmt::{Debug, Display, Write},
};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_till1, take_until, take_while},
    character::complete::{anychar, char},
    combinator::{all_consuming, fail, map, map_opt, recognize, value},
    multi::many0,
    sequence::{delimited, preceded, terminated},
    IResult, InputIter,
};

use crate::error::MyError;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Comment,
    TemplateStart,
    TemplateEnd,
    LinkStart,
    LinkEnd,
    ElementStart(String, bool),
    ElementEnd(String),
    Content(&'a str),
}

fn quick_escape(s: &str) -> String {
    s.replace('\\', "\\\\").replace('\n', "\\n")
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Comment => f.write_char('!'),
            Token::TemplateStart => f.write_char('{'),
            Token::TemplateEnd => f.write_char('}'),
            Token::LinkStart => f.write_char('['),
            Token::LinkEnd => f.write_char(']'),
            Token::ElementStart(name, false) => f.write_fmt(format_args!("<{}", name)),
            Token::ElementStart(name, true) => f.write_fmt(format_args!("<>{}", name)),
            Token::ElementEnd(name) => f.write_fmt(format_args!(">{}", name)),
            Token::Content(text) => f.write_fmt(format_args!(".{}", quick_escape(text))),
        }
    }
}

fn is_space(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\r' || c == '\n'
}

fn sp(i: &str) -> IResult<&str, &str> {
    take_while(is_space)(i)
}

fn is_invalid_for_xml_name(c: char) -> bool {
    match c {
        '&' => true,
        '<' => true,
        '>' => true,
        '?' => true,
        '"' => true,
        '\'' => true,
        '/' => true,
        ';' => true,
        '#' => true,
        _ => is_space(c),
    }
}

// fn is_invalid_for_xml_ident(c: char) -> bool {
//     is_invalid_for_xml_name(c) || c == ':'
// }

// fn parse_ident(i: &str) -> IResult<&str, &str> {
//     take_till1(is_invalid_for_xml_ident)(i)
// }

fn parse_name(i: &str) -> IResult<&str, &str> {
    take_till1(is_invalid_for_xml_name)(i)
}

fn parse_name_lower(i: &str) -> IResult<&str, String> {
    map(parse_name, |s| s.to_ascii_lowercase())(i)
}

fn is_special(c: char) -> bool {
    c == '<' || c == '{' || c == '}' || c == '[' || c == ']'
}

fn parse_content(i: &str) -> IResult<&str, &str> {
    take_till(is_special)(i)
}

fn parse_token(i: &str) -> IResult<&str, Token> {
    alt((
        preceded(char('<'), parse_langle),
        map(tag("{{"), |_| Token::TemplateStart),
        map(tag("}}"), |_| Token::TemplateEnd),
        map(tag("[["), |_| Token::LinkStart),
        map(tag("]]"), |_| Token::LinkEnd),
        map(recognize(preceded(anychar, parse_content)), Token::Content),
    ))(i)
}

// preceding: '<'
fn parse_langle(i: &str) -> IResult<&str, Token> {
    alt((
        preceded(char('!'), parse_comment_like),
        preceded(char('?'), parse_instr),
        preceded(char('/'), parse_end),
        parse_begin,
    ))(i)
}

fn parse_comment_like(i: &str) -> IResult<&str, Token> {
    value(
        Token::Comment,
        alt((
            preceded(tag("--"), take_until("-->")),
            preceded(tag("[CDATA["), take_until("]]>")),
        )),
    )(i)
}

fn parse_instr(i: &str) -> IResult<&str, Token> {
    value(Token::Comment, take_until("?>"))(i)
}

fn parse_end(i: &str) -> IResult<&str, Token> {
    map(
        delimited(sp, parse_name, terminated(sp, char('>'))),
        |n| Token::ElementEnd(n.into()),
    )(i)
}

fn parse_begin(i: &str) -> IResult<&str, Token> {
    let (rest, name) = map_opt(
        parse_name_lower,
        |n| Some(n).filter(|it| validate_name(it.as_str())),
    )(i)?;
    let mut st = ('\0', false);
    for (i, c) in rest.iter_indices() {
        match (c, st.1) {
            ('>', false) => unsafe {
                // safety: iter_indices `i` is in bounds and at a utf-8 boundary. '<' is one byte,
                // so i+1 will be a utf-8 boundary unless `i` is the last index. In that case,
                // s[s.len()..] is also a valid zero-length slice
                let rest2 = rest.get_unchecked(i + 1..);
                return Ok((rest2, Token::ElementStart(name, st.0 == '/')));
            },
            ('<', false) => unsafe {
                // safety: iter_indices `i` is in bounds and at a utf-8 boundary
                let rest2 = rest.get_unchecked(i..);
                return Ok((rest2, Token::ElementStart(name, st.0 == '/')));
            },
            ('"', b) => st = ('"', !b),
            (ch, _) => {
                if !is_space(ch) {
                    st = (ch, st.1)
                }
            }
        }
    }
    fail("")
}

pub fn tokenize(text: &str) -> Result<Vec<Token>, MyError> {
    let (_, tokens) = all_consuming(many0(parse_token))(text)
        .map_err(MyError::wrap)?;
    Ok(tokens)
}

fn validate_name(name: &str) -> bool {
    match name {
        "categorytree" => true,
        "ce" => true,
        "charinsert" => true,
        "chem" => true,
        "gallery" => true,
        "graph" => true,
        "hiero" => true,
        "imagemap" => true,
        "indicator" => true,
        "inputbox" => true,
        "langconvert" => true,
        "mapframe" => true,
        "maplink" => true,
        "math" => true,
        "nowiki" => true,
        "poem" => true,
        "pre" => true,
        "ref" => true,
        "references" => true,
        "score" => true,
        "section" => true,
        "source" => true,
        "syntaxhighlight" => true,
        "templatedata" => true,
        "templatestyles" => true,
        "timeline" => true,
        // End https://en.wikipedia.org/wiki/Special:Version#mw-version-parser-extensiontags
        "html" => true,
        "head" => true,
        "title" => true,
        "base" => true,
        "link" => true,
        "meta" => true,
        "style" => true,
        "body" => true,
        "article" => true,
        // "section" => true,
        "nav" => true,
        "aside" => true,
        "h1" => true,
        "h2" => true,
        "h3" => true,
        "h4" => true,
        "h5" => true,
        "h6" => true,
        "hgroup" => true,
        "header" => true,
        "footer" => true,
        "address" => true,
        "p" => true,
        "hr" => true,
        // "pre" => true,
        "blockquote" => true,
        "ol" => true,
        "ul" => true,
        "menu" => true,
        "li" => true,
        "dl" => true,
        "dt" => true,
        "dd" => true,
        "figure" => true,
        "figcaption" => true,
        "main" => true,
        "div" => true,
        "a" => true,
        "em" => true,
        "strong" => true,
        "small" => true,
        "s" => true,
        "cite" => true,
        "q" => true,
        "dfn" => true,
        "abbr" => true,
        "ruby" => true,
        "rt" => true,
        "rp" => true,
        "data" => true,
        "time" => true,
        "code" => true,
        "var" => true,
        "samp" => true,
        "kbd" => true,
        "sub" => true,
        "sup" => true,
        "i" => true,
        "b" => true,
        "u" => true,
        "mark" => true,
        "bdi" => true,
        "bdo" => true,
        "span" => true,
        "br" => true,
        "wbr" => true,
        "ins" => true,
        "del" => true,
        "picture" => true,
        // "source" => true,
        "img" => true,
        "iframe" => true,
        "embed" => true,
        "object" => true,
        "video" => true,
        "audio" => true,
        "track" => true,
        "map" => true,
        "area" => true,
        "table" => true,
        "caption" => true,
        "colgroup" => true,
        "col" => true,
        "tbody" => true,
        "thead" => true,
        "tfoot" => true,
        "tr" => true,
        "td" => true,
        "th" => true,
        "form" => true,
        "label" => true,
        "input" => true,
        "button" => true,
        "select" => true,
        "datalist" => true,
        "optgroup" => true,
        "option" => true,
        "textarea" => true,
        "output" => true,
        "progress" => true,
        "meter" => true,
        "fieldset" => true,
        "legend" => true,
        "details" => true,
        "summary" => true,
        "dialog" => true,
        "script" => true,
        "noscript" => true,
        "template" => true,
        "slot" => true,
        "canvas" => true,
        "applet" => true,
        "acronym" => true,
        "bgsound" => true,
        "dir" => true,
        "noframes" => true,
        "isindex" => true,
        "keygen" => true,
        "listing" => true,
        "menuitem" => true,
        "nextid" => true,
        "noembed" => true,
        "param" => true,
        "plaintext" => true,
        "rb" => true,
        "rtc" => true,
        "strike" => true,
        "xmp" => true,
        "basefont" => true,
        "big" => true,
        "blink" => true,
        "center" => true,
        "font" => true,
        "multicol" => true,
        "nobr" => true,
        "spacer" => true,
        "tt" => true,
        "marquee" => true,
        "frameset" => true,
        "frame" => true,
        _ => false,
    }
}
