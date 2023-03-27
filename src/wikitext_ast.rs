use nom::{
    branch::alt,
    combinator::{all_consuming, map, value},
    error::ParseError,
    multi::{count, many0, many1, many_till},
    sequence::delimited,
    IResult,
};

use crate::Result;
use crate::{
    wikitext::{is_void_element, tokenize, Token},
    Error, ErrorKind,
};

#[derive(Clone)]
pub enum Node {
    Document(Vec<Node>),
    Element(String, Vec<Node>),
    Template(Vec<Node>),
    Link(Vec<Node>),
    Argument(Vec<Node>),
    Content(String),
}

fn satisfy<E, F>(cond: F) -> impl Fn(&[E]) -> IResult<&[E], &E>
where
    F: Fn(&E) -> bool,
{
    move |i| {
        i.split_first()
            .and_then(|(first, rest)| {
                if cond(first) {
                    Some((rest, first))
                } else {
                    None
                }
            })
            .ok_or(nom::Err::Error(nom::error::Error::from_error_kind(
                i,
                nom::error::ErrorKind::Satisfy,
            )))
    }
}

fn take_next<E>(i: &[E]) -> IResult<&[E], &E> {
    i.split_first()
        .map(|(first, rest)| (rest, first))
        .ok_or(nom::Err::Error(nom::error::Error::from_error_kind(
            i,
            nom::error::ErrorKind::Char,
        )))
}

fn map_next<E, O, F>(mapper: F) -> impl Fn(&[E]) -> IResult<&[E], O>
where
    F: Fn(&E) -> Option<O>,
{
    move |i| {
        i.split_first()
            .and_then(|(first, rest)| {
                let o = mapper(first)?;
                Some((rest, o))
            })
            .ok_or(nom::Err::Error(nom::error::Error::from_error_kind(
                i,
                nom::error::ErrorKind::MapOpt,
            )))
    }
}

fn parse_content<'a>(i: &'a [Token]) -> IResult<&'a [Token<'a>], Vec<&'a str>> {
    many1(map_next(|token| match token {
        Token::Comment => Some(""),
        Token::ElementStart(name, _) if name == "br" => Some("\n"),
        Token::Content(text) => Some(text),
        _ => None,
    }))(i)
}

fn parse_any_for_structure<'a>(i: &'a [Token]) -> IResult<&'a [Token<'a>], Vec<Node>> {
    alt((
        map(parse_content, |strings| {
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
        count(parse_structure, 1),
    ))(i)
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

fn parse_template<'a>(i: &'a [Token]) -> IResult<&'a [Token<'a>], Node> {
    let mut parser = delimited(
        satisfy(|tok| matches!(tok, Token::TemplateStart)),
        many0(parse_any_for_structure),
        satisfy(|tok| matches!(tok, Token::TemplateEnd)),
    );
    parser(i).map(|(remainder, children)| (remainder, Node::Template(organize_children(children))))
}

fn parse_link<'a>(i: &'a [Token]) -> IResult<&'a [Token<'a>], Node> {
    let mut parser = delimited(
        satisfy(|tok| matches!(tok, Token::LinkStart)),
        many0(parse_any_for_structure),
        satisfy(|tok| matches!(tok, Token::LinkEnd)),
    );
    parser(i).map(|(remainder, children)| (remainder, Node::Link(organize_children(children))))
}

fn parse_element<'a>(i: &'a [Token]) -> IResult<&'a [Token<'a>], Node> {
    let (rest, (name, is_self_closing)) = map_next(|tok| match tok {
        Token::ElementStart(n, i) => Some((n.clone(), *i)),
        _ => None,
    })(i)?;
    if is_self_closing || is_void_element(name.as_str()) {
        Ok((rest, Node::Element(name, vec![])))
    } else {
        value(
            Node::Element(name.clone(), vec![]),
            many_till(
                take_next,
                satisfy(|tok| matches!(tok, Token::ElementEnd(n) if n == &name)),
            ),
        )(rest)
    }
}

fn parse_structure<'a>(i: &'a [Token]) -> IResult<&'a [Token<'a>], Node> {
    alt((parse_template, parse_link, parse_element))(i)
}

fn parse_any<'a>(i: &'a [Token]) -> IResult<&'a [Token<'a>], Option<Node>> {
    alt((
        map(parse_structure, Some),
        map(parse_content, |s| Some(Node::Content(s.join("")))),
        // Can only ignore unbalanced end tags at depth 0
        value(
            None,
            satisfy(|tok| {
                matches!(
                    tok,
                    Token::TemplateEnd | Token::LinkEnd | Token::ElementEnd(_)
                )
            }),
        ),
    ))(i)
}

fn parse_everything<'a>(i: &'a [Token]) -> IResult<&'a [Token<'a>], Node> {
    map(all_consuming(many0(parse_any)), |nodes| {
        Node::Document(nodes.into_iter().flatten().collect())
    })(i)
}

pub fn parse_document(text: &str) -> Result<Node> {
    let tokens = tokenize(text)?;
    let tokens_len = tokens.len();
    let (_, doc) = parse_everything(&tokens).map_err(|e| {
        Error::from(ErrorKind::WikitextParseError(
            e.map_input(|i| tokens_len - i.len()).to_string(),
        ))
    })?;
    Ok(doc)
}
