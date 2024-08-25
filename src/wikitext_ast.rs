use nom::{
    branch::alt, bytes::complete::tag, character::complete::char, combinator::value,
    sequence::delimited, IResult,
};

use crate::{
    wikitext::{is_syntax_override, is_void_element, Token},
    ErrorKind, Result,
};

fn parse_content_entity(i: &str) -> IResult<&str, &str> {
    delimited(
        char('&'),
        alt((
            value("<", tag("lt")),
            value(">", tag("gt")),
            value("&", tag("amp")),
            value("\"", tag("quot")),
            value("'", tag("apos")),
            value(" ", tag("nbsp")),
        )),
        char(';'),
    )(i)
}

fn parse_content_entity_aware(i: &str) -> String {
    let mut full = String::new();
    let mut input = i;
    loop {
        match input.find('&') {
            None => {
                full += input;
                break;
            }
            Some(j) => {
                let (pre, ent_plus) = input.split_at(j);
                full += pre;
                if let Ok((post, ent)) = parse_content_entity(ent_plus) {
                    full += ent;
                    input = post;
                } else {
                    full += &ent_plus[..1];
                    input = &ent_plus[1..];
                }
            }
        }
    }
    full
}

#[derive(Clone)]
pub enum Node {
    Document(Vec<Node>),
    Element(String, Vec<Node>),
    Template(Vec<Node>),
    Link(Vec<Node>),
    Header(usize, Vec<Node>),
    Argument(Vec<Node>),
    Content(String),
}

type ParseResult<'a, T> = Result<(&'a [Token<'a>], Vec<&'a Token<'a>>, T)>;

trait MapWithin<I, O> {
    type Output;

    fn map_within<F>(self, f: F) -> Self::Output
    where
        F: FnOnce(I) -> O;
}

impl<'a, T, U> MapWithin<T, U> for ParseResult<'a, T> {
    type Output = ParseResult<'a, U>;

    fn map_within<F>(self, f: F) -> Self::Output
    where
        F: FnOnce(T) -> U,
    {
        self.map(|(i, u, x)| (i, u, f(x)))
    }
}

fn parse_content<'a>(
    i: &'a [Token],
    unclosed: Vec<&'a Token>,
    overridden: bool,
) -> ParseResult<'a, Vec<String>> {
    let strings: Vec<_> = i
        .iter()
        .map_while(|token| match token {
            Token::Comment => Some("".into()),
            Token::Content(text) => Some(parse_content_entity_aware(text)),
            Token::TemplateStart | Token::TemplateEnd | Token::LinkStart | Token::LinkEnd => {
                if overridden {
                    Some(token.to_string())
                } else {
                    None
                }
            }
            _ => None,
        })
        .collect();
    match strings.len() {
        0 => Err(ErrorKind::ASTParseError("parse_content consumed zero".into(), i.len()).into()),
        n => Ok((&i[n..], unclosed, strings)),
    }
}

fn parse_any_for_structure<'a>(
    i: &'a [Token],
    unclosed: Vec<&'a Token>,
    overridden: bool,
) -> ParseResult<'a, Vec<Node>> {
    if let Some(first) = i.first() {
        match (overridden, first) {
            (false, Token::TemplateStart | Token::LinkStart | Token::HeaderStart(_)) => {
                parse_structure(i, unclosed).map_within(|n| vec![n])
            }
            (false, Token::TemplateEnd | Token::LinkEnd | Token::HeaderEnd(_)) => {
                Err(ErrorKind::ASTParseError(format!("parse_any_for_structure first={:?}", first), i.len()).into())
            }
            (_, Token::ElementStart(_, _)) => parse_structure(i, unclosed).map_within(|n| vec![n]),
            (_, Token::ElementEnd(name)) => match name.as_str() {
                "br" => Ok((&i[1..], unclosed, vec![Node::Content("\n".to_string())])),
                n if is_void_element(n) => Ok((
                    &i[1..],
                    unclosed,
                    vec![Node::Element(name.to_owned(), vec![])],
                )),
                _ => {
                    Err(ErrorKind::ASTParseError(format!("parse_any_for_structure first={:?}", first), i.len()).into())
                }
            },
            _ => parse_content(i, unclosed, overridden).map_within(|strings| {
                let full = strings.join("");
                let last = full.len();
                let iterator = full
                    .char_indices()
                    .filter(|(_, c)| *c == '|')
                    .map(|(i, _)| i);
                let iterator = std::iter::once(0usize).chain(iterator);
                let mut iterator = iterator.peekable();
                let mut nodes = vec![];
                while let Some(index) = iterator.next() {
                    let end_index = iterator.peek().map_or(last, |x| *x);
                    nodes.push(Node::Content(full[index..end_index].to_string()))
                }

                nodes
            }),
        }
    } else {
        Ok((i, unclosed, vec![]))
    }
}

fn matching_pos(unclosed: &[&Token], current: &Token, overridden: bool) -> Option<usize> {
    match (overridden, current) {
        (false, Token::TemplateEnd) => unclosed
            .iter()
            .rposition(|c| matches!(c, Token::TemplateStart)),
        (false, Token::LinkEnd) => unclosed.iter().rposition(|c| matches!(c, Token::LinkStart)),
        (false, Token::HeaderEnd(_)) => unclosed
            .iter()
            .rposition(|c| matches!(c, Token::HeaderStart(_))),
        (_, Token::ElementEnd(name)) => unclosed
            .iter()
            .rposition(|c| matches!(c, Token::ElementStart(n, false) if n == name)),
        _ => None,
    }
}

fn organize_children(nodes: Vec<Vec<Node>>) -> Vec<Node> {
    let mut children = vec![];
    for node in nodes.into_iter().flatten() {
        match &node {
            Node::Content(c) if c.starts_with('|') => children.push(vec![node]),
            _ => {
                if let Some(last) = children.last_mut() {
                    last.push(node)
                } else {
                    children.push(vec![node])
                }
            }
        }
    }
    children.into_iter().map(Node::Argument).collect()
}

fn parse_structure_inner<'a>(i: &'a [Token], u: Vec<&'a Token>) -> ParseResult<'a, Vec<Node>> {
    let mut input = i;
    let mut unclosed = u;
    let mut children = vec![];
    while !input.is_empty() {
        let (first, rest) = input.split_first().unwrap();
        let stack_size = unclosed.len();
        let overridden = unclosed.iter().any(
            |tok| matches!(tok, Token::ElementStart(n, false) if is_syntax_override(n.as_str())),
        );
        match matching_pos(&unclosed, first, overridden) {
            None => {
                let (inxt, unxt, nodes) = parse_any_for_structure(input, unclosed, overridden)?;
                input = inxt;
                unclosed = unxt;
                children.push(nodes);
            }
            Some(n) if n == stack_size - 1 => {
                // Natural close found, consume it
                input = rest;
                unclosed.pop();
                break;
            }
            Some(_) => {
                // Close for a different opener found, don't consume
                unclosed = unclosed.split_last().unwrap().1.to_vec();
                break;
            }
        }
    }
    Ok((input, unclosed, organize_children(children)))
}

fn parse_structure<'a>(i: &'a [Token], u: Vec<&'a Token>) -> ParseResult<'a, Node> {
    let mut unclosed = u.clone();
    if let Some((first, rest)) = i.split_first() {
        match first {
            Token::TemplateStart => {
                unclosed.push(first);
                parse_structure_inner(rest, unclosed).map_within(Node::Template)
            }
            Token::LinkStart => {
                unclosed.push(first);
                parse_structure_inner(rest, unclosed).map_within(Node::Link)
            }
            Token::HeaderStart(marker) => {
                let lvl = marker.chars().filter(|c| *c == '=').count();
                unclosed.push(first);
                parse_structure_inner(rest, unclosed)
                    .map_within(|children| Node::Header(lvl, children))
            }
            Token::ElementStart(name, is_self_closing) => {
                if *is_self_closing || is_void_element(name.as_str()) {
                    Ok((rest, unclosed, Node::Element(name.to_owned(), vec![])))
                } else {
                    unclosed.push(first);
                    parse_structure_inner(rest, unclosed)
                        .map_within(|nl| Node::Element(name.to_owned(), nl))
                }
            }
            _ => Err(
                ErrorKind::ASTParseError("parse_structure consumed zero".into(), i.len()).into(),
            ),
        }
    } else {
        Err(ErrorKind::ASTParseError("parse_structure EOF".into(), 0).into())
    }
}

fn parse_any<'a>(i: &'a [Token]) -> ParseResult<'a, Option<Node>> {
    let unclosed = vec![];
    match i.first().unwrap() {
        Token::TemplateStart
        | Token::LinkStart
        | Token::ElementStart(_, _)
        | Token::HeaderStart(_) => parse_structure(i, unclosed).map_within(Some),
        Token::TemplateEnd | Token::LinkEnd | Token::HeaderEnd(_) => Ok((&i[1..], unclosed, None)),
        Token::ElementEnd(name) => {
            if is_void_element(name.as_str()) {
                Ok((
                    &i[1..],
                    unclosed,
                    Some(Node::Element(name.to_owned(), vec![])),
                ))
            } else {
                Ok((&i[1..], unclosed, None))
            }
        }
        Token::Content(_) | Token::Comment => {
            parse_content(i, unclosed, false).map_within(|sl| Some(Node::Content(sl.join(""))))
        }
    }
}

fn parse_everything(i: &[Token]) -> Result<Node> {
    let mut input = i;
    let mut children = vec![];
    while !input.is_empty() {
        let (inxt, _, mn) = parse_any(input)?;
        input = inxt;
        if let Some(node) = mn {
            children.push(node);
        }
    }
    Ok(Node::Document(children))
}

pub fn parse(tokens: Vec<Token>) -> Result<Node> {
    let tokens_len = tokens.len();
    parse_everything(&tokens).map_err(|err| match err.kind() {
        ErrorKind::ASTParseError(msg, n) => {
            ErrorKind::ASTParseError(msg.to_owned(), tokens_len - *n).into()
        }
        _ => err,
    })
}
