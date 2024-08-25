use std::{
    cmp::{Eq, PartialEq},
    fmt::{Debug, Display},
    iter::Peekable,
};

use lazy_static::lazy_static;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_till1, take_until, take_while},
    character::complete::{anychar, char},
    combinator::{all_consuming, fail, map, map_opt, recognize, value},
    multi::many0,
    sequence::{delimited, preceded, terminated},
    IResult, InputIter,
};
use regex::Regex;

use crate::{Error, ErrorKind, Result};

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

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Comment => f.write_str("<!-- -->"),
            Token::TemplateStart => f.write_str("{{"),
            Token::TemplateEnd => f.write_str("}}"),
            Token::LinkStart => f.write_str("[["),
            Token::LinkEnd => f.write_str("]]"),
            Token::ElementStart(name, false) => f.write_fmt(format_args!("<{}>", name)),
            Token::ElementStart(name, true) => f.write_fmt(format_args!("<{} />", name)),
            Token::ElementEnd(name) => f.write_fmt(format_args!("</{}>", name)),
            Token::Content(text) => f.write_str(text),
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
            delimited(tag("--"), take_until("-->"), tag("-->")),
            delimited(tag("[CDATA["), take_until("]]>"), tag("]]>")),
        )),
    )(i)
}

fn parse_instr(i: &str) -> IResult<&str, Token> {
    value(Token::Comment, take_until("?>"))(i)
}

fn parse_end(i: &str) -> IResult<&str, Token> {
    map(
        delimited(
            sp,
            map_opt(parse_name_lower, |n| {
                Some(n).filter(|it| validate_name(it.as_str()))
            }),
            terminated(sp, char('>')),
        ),
        Token::ElementEnd,
    )(i)
}

fn parse_begin(i: &str) -> IResult<&str, Token> {
    let (rest, name) = map_opt(parse_name_lower, |n| {
        Some(n).filter(|it| validate_name(it.as_str()))
    })(i)?;
    let mut last = '\0';
    let mut unclosed_quote = false;
    for (i, c) in rest.iter_indices() {
        match (c, unclosed_quote) {
            ('>', false) => unsafe {
                // safety: iter_indices `i` is in bounds and at a utf-8 boundary. '<' is one byte,
                // so i+1 will be a utf-8 boundary unless `i` is the last index. In that case,
                // s[s.len()..] is also a valid zero-length slice
                let rest2 = rest.get_unchecked(i + 1..);
                return Ok((rest2, Token::ElementStart(name, last == '/')));
            },
            ('<', false) => unsafe {
                // safety: iter_indices `i` is in bounds and at a utf-8 boundary
                let rest2 = rest.get_unchecked(i..);
                return Ok((rest2, Token::ElementStart(name, last == '/')));
            },
            ('"', _) => {
                unclosed_quote = !unclosed_quote;
            }
            _ => {},
        }
        if !is_space(c) {
            last = c;
        }
    }
    fail("")
}

pub fn tokenize(text: &str) -> Result<Vec<Token>> {
    let (_, tokens) = all_consuming(many0(parse_token))(text)
        .map_err(|e| Error::from(ErrorKind::WikitextParseError(e.to_string())))?;
    Ok(tokens)
}

struct WikiPageContext<'a> {
    unclosed: Vec<Token<'a>>,
    unclosed_syntax_overrides: usize,
    unclosed_refs: usize,
    verbose: bool,
}

pub fn is_syntax_override(name: &str) -> bool {
    matches!(name, "nowiki" | "pre" | "code" | "math" | "chem")
}

pub fn is_void_element(name: &str) -> bool {
    // https://developer.mozilla.org/en-US/docs/Glossary/Void_element
    matches!(
        name,
        "area"
            | "base"
            | "br"
            | "col"
            | "embed"
            | "hr"
            | "img"
            | "input"
            | "keygen"
            | "link"
            | "meta"
            | "param"
            | "source"
            | "track"
            | "wbr"
    )
}

impl<'a> WikiPageContext<'a> {
    fn new(verbose: bool) -> WikiPageContext<'a> {
        WikiPageContext {
            unclosed: Vec::new(),
            unclosed_syntax_overrides: 0,
            unclosed_refs: 0,
            verbose,
        }
    }

    fn update(&mut self, token: Token<'a>) {
        match (self.unclosed_syntax_overrides, token.clone()) {
            (_, Token::ElementStart(name, false)) => {
                if is_void_element(name.as_str()) {
                    return;
                } else if is_syntax_override(name.as_str()) {
                    self.unclosed_syntax_overrides += 1;
                } else if name == "ref" {
                    self.unclosed_refs += 1;
                }
                self.unclosed.push(token);
            }
            (_, Token::ElementEnd(name)) => {
                while let Some(t2) = self.unclosed.pop() {
                    if matches!(t2.clone(), Token::ElementStart(n2, false) if n2 == name) {
                        if is_syntax_override(name.as_str()) {
                            self.unclosed_syntax_overrides -= 1;
                        } else if name == "ref" {
                            self.unclosed_refs -= 1;
                        }
                        break;
                    } else if self.verbose {
                        eprintln!(
                            "Got {:?} while searching for closing tag to {:?}",
                            token, t2
                        );
                    }
                }
            }
            (0, Token::TemplateStart) => {
                self.unclosed.push(token);
            }
            (0, Token::LinkStart) => {
                self.unclosed.push(token);
            }
            (0, Token::TemplateEnd) => {
                while let Some(t2) = self.unclosed.pop() {
                    if t2 == Token::TemplateStart {
                        break;
                    } else if self.verbose {
                        eprintln!(
                            "Got {:?} while searching for closing tag to {:?}",
                            token, t2
                        );
                    }
                }
            }
            (0, Token::LinkEnd) => {
                while let Some(t2) = self.unclosed.pop() {
                    if t2 == Token::LinkStart {
                        break;
                    } else if self.verbose {
                        eprintln!(
                            "Got {:?} while searching for closing tag to {:?}",
                            token, t2
                        );
                    }
                }
            }
            _ => {}
        }
    }
}

pub fn get_links(text: &str) -> Result<Vec<String>> {
    let tokens = tokenize(text)?;
    let mut result = Vec::new();
    let mut open_links = Vec::new();
    let mut context = WikiPageContext::new(false);
    for tok in tokens {
        match tok {
            Token::ElementStart(_, _) | Token::ElementEnd(_) => {
                context.update(tok);
            }
            Token::TemplateStart => {
                context.update(tok);
                for s in open_links.iter_mut() {
                    *s += "{{";
                }
            }
            Token::TemplateEnd => {
                context.update(tok);
                for s in open_links.iter_mut() {
                    *s += "}}";
                }
            }
            Token::LinkStart => {
                for s in open_links.iter_mut() {
                    *s += "[[";
                }
                if context.unclosed_syntax_overrides == 0 && context.unclosed_refs == 0 {
                    open_links.push(String::from(""));
                }
            }
            Token::LinkEnd => {
                if context.unclosed_syntax_overrides == 0 && context.unclosed_refs == 0 {
                    if let Some(link) = open_links.pop() {
                        result.push(link);
                    }
                }
                for s in open_links.iter_mut() {
                    *s += "]]";
                }
            }
            Token::Content(c) => {
                for s in open_links.iter_mut() {
                    *s += c;
                }
            }
            _ => continue,
        }
    }
    Ok(result)
}

struct TemplateIterator<'a, F>
where
    F: Fn(&str) -> bool,
{
    inner: Peekable<std::vec::IntoIter<Token<'a>>>,
    open_tmpls: Vec<Option<String>>,
    context: WikiPageContext<'a>,
    acceptor: F,
}

impl<'a, F> TemplateIterator<'a, F>
where
    F: Fn(&str) -> bool,
{
    fn new(tokens: Vec<Token<'a>>, acceptor: F) -> TemplateIterator<'a, F> {
        TemplateIterator {
            inner: tokens.into_iter().peekable(),
            open_tmpls: Vec::new(),
            context: WikiPageContext::new(false),
            acceptor,
        }
    }
}

impl<'a, F> Iterator for TemplateIterator<'a, F>
where
    F: Fn(&str) -> bool,
{
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(tok) = self.inner.next() {
            match tok {
                Token::ElementStart(_, _) | Token::ElementEnd(_) => {
                    self.context.update(tok);
                }
                Token::TemplateStart => {
                    for s in self.open_tmpls.iter_mut().flatten() {
                        *s += "{{";
                    }
                    if self.context.unclosed_syntax_overrides == 0 {
                        if matches!(self.inner.peek(), Some(Token::Content(c)) if (self.acceptor)(c))
                        {
                            self.open_tmpls.push(Some(String::from("")));
                        } else {
                            self.open_tmpls.push(None);
                        }
                    }
                }
                Token::TemplateEnd => {
                    if self.context.unclosed_syntax_overrides == 0 {
                        if let Some(Some(tmpl)) = self.open_tmpls.pop() {
                            return Some(tmpl);
                        }
                    }
                    for s in self.open_tmpls.iter_mut().flatten() {
                        *s += "}}";
                    }
                }
                Token::LinkStart => {
                    for s in self.open_tmpls.iter_mut().flatten() {
                        *s += "[[";
                    }
                }
                Token::LinkEnd => {
                    for s in self.open_tmpls.iter_mut().flatten() {
                        *s += "]]";
                    }
                }
                Token::Content(c) => {
                    for s in self.open_tmpls.iter_mut().flatten() {
                        *s += c;
                    }
                }
                _ => continue,
            }
        }
        None
    }
}

pub fn get_cite_urls(text: &str) -> Result<Vec<String>> {
    let tokens = tokenize(text)?;
    let tmpl_iter = TemplateIterator::new(tokens, |title| title.starts_with("cite web"));
    lazy_static! {
        static ref URL_RE: Regex = Regex::new(r"\|\s*url\s*=\s*https?://(?P<url>[^|]+)").unwrap();
    }

    let result: Vec<String> = tmpl_iter
        .into_iter()
        .flat_map(|s| {
            URL_RE
                .captures(s.as_str())
                .and_then(|cap| cap.name("url"))
                .map(|mat| mat.as_str().trim_end().to_owned())
        })
        .collect();
    Ok(result)
}

pub fn is_disambiguation_page(text: &str) -> Result<bool> {
    let tokens = tokenize(text)?;
    let mut tmpl_iter = TemplateIterator::new(tokens, is_disambiguation_tmpl);
    Ok(tmpl_iter.next().is_some())
}

pub fn get_distinguish_hatnotes(text: &str) -> Result<Vec<String>> {
    let tokens = tokenize(text)?;
    let mut tmpl_iter = TemplateIterator::new(tokens, is_distinguish_hatnote);
    let mut result = vec![];
    while let Some(tmpl) = tmpl_iter.next() {
        if tmpl_iter.open_tmpls.iter().all(|e| e.is_none()) {
            result.push(tmpl);
        }
    }
    Ok(result)
}

pub fn get_templates_by_name(text: &str, names: Vec<String>) -> Result<Vec<String>> {
    let tokens = tokenize(text)?;
    let mut tmpl_iter = TemplateIterator::new(tokens, |title| tmpl_title_is_in_names(title, &names));
    let mut result = vec![];
    while let Some(tmpl) = tmpl_iter.next() {
        if tmpl_iter.open_tmpls.iter().all(|e| e.is_none()) {
            result.push(tmpl);
        }
    }
    Ok(result)
}

pub fn get_first_infobox_title(text: &str) -> Result<Option<String>> {
    let tokens = tokenize(text)?;
    let mut tmpl_iter = TemplateIterator::new(tokens, |title| {
        title.starts_with("Infobox ") || title.starts_with("infobox ")
    });
    while let Some(tmpl) = tmpl_iter.next() {
        if tmpl_iter.open_tmpls.iter().all(|e| e.is_none()) {
            let (title, _) = tmpl.split_once('|').unwrap_or((&tmpl, ""));
            let title = title.get(8..).expect("(\"Infobox \" + c).get(8..) === Some(c)");
            return Ok(Some(title.trim().to_string()))
        }
    }
    Ok(None)
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

fn parse_tmpl_title(i: &str) -> IResult<&str, String> {
    let (rest, title) =
        take_till(|c| matches!(c, '|' | '#' | '<' | '>' | '[' | ']' | '{' | '}' | '_'))(i)?;
    let mut title_s = title.to_owned();
    if !title_s.is_empty() {
        if let Some(prefix) = title_s.get_mut(0..1) {
            prefix.make_ascii_uppercase();
        }
    }
    Ok((rest, title_s))
}

fn tmpl_title_is_in_names(title: &str, names: &[String]) -> bool {
    if let Ok((_, title_u1)) = parse_tmpl_title(title) {
        names.contains(&title_u1)
    } else {
        false
    }
}

fn is_distinguish_hatnote(title: &str) -> bool {
    if let Ok((_, title_u1)) = parse_tmpl_title(title) {
        matches!(
            title_u1.as_str(),
            "Hatnote"
                | "For"
                | "For-text"
                | "For-multi"
                | "About"
                | "Other uses"
                | "Other uses of"
                | "Redirect-distinguish-for"
                | "About-distinguish"
                | "About-distinguish-text"
                | "Other people"
                | "About other people"
                | "Similar names"
                | "Other storms"
                | "Other places"
                | "Other ships"
                | "Redirect"
                | "Redirect2"
                | "Redirect-multi"
                | "Redirect-several"
                | "Redirect-synonym"
                | "Redirect-distinguish"
                | "Redirect-distinguish-text"
                | "Technical reasons"
                | "Distinguish"
        )
    } else {
        false
    }
}

fn is_disambiguation_tmpl(title: &str) -> bool {
    if let Ok((_, title_u1)) = parse_tmpl_title(title) {
        matches!(
            title_u1.as_str(),
            "Dab"
                | "Disamb"
                | "Numdisambig"
                | "Bio-dab"
                | "Hndisambig"
                | "Numdab"
                | "Shortcut disambig"
                | "WP-disambig"
                | "CJKVdab"
                | "Meta disambig"
                | "Disambig-plants"
                | "Geodab"
                | "Hndab"
                | "Geo-dis"
                | "Wikipedia disambiguation"
                | "Sia"
                | "Roaddis"
                | "LatinNameDisambig"
                | "SpeciesLatinNameDisambig"
                | "DAB"
                | "Letter-NumberCombdisambig"
                | "Genus disambig"
                | "WP disambig"
                | "HnDis"
                | "Set index"
                | "Surnames"
                | "Dbig"
                | "Disambig"
                | "Disambiguation page"
                | "Hospitaldis"
                | "Taxonomic authorities disambiguation"
                | "Letter-NumberCombinationDisambiguation"
                | "Airport disambig"
                | "Callsigndis"
                | "Disambig-Chinese-char-title"
                | "MolFormDisambig"
                | "Mathematics disambiguation"
                | "Schooldis"
                | "Personal name disambiguation"
                | "Mathdab"
                | "SIA"
                | "Mountainindex"
                | "Lakeindex"
                | "Mil-unit-disambig"
                | "Schooldab"
                | "Chemdisambig"
                | "Geodisambig"
                | "Chemistry disambiguation"
                | "Molecular formula disambiguation"
                | "MolFormIndex"
                | "LNCD"
                | "Disam"
                | "Letter-Number combination disambiguation"
                | "Letter-NumberCombDisambig"
                | "DisambigName"
                | "DisambigNm"
                | "DisambigN"
                | "Species disambiguation"
                | "Media index"
                | "Project disambiguation"
                | "Sportindex"
                | "Roadindex"
                | "Shipindex"
                | "Set-index"
                | "Case law disambiguation"
                | "Chinese title disambig"
                | "Setindex"
                | "HNDIS"
                | "Set-index article"
                | "First name"
                | "Forename"
                | "Hndis"
                | "Personal name"
                | "Geodis"
                | "Numberdis"
                | "Disambig misspelling"
                | "Begriffsklärung"
                | "Music disambig"
                | "Station dab"
                | "Mil-unit-dis"
                | "Letter-Number Combination Disambiguation"
                | "Portal disambig"
                | "DisambigG"
                | "DisambigGeo"
                | "Disambiggeo"
                | "Chemistry set index"
                | "Math dab"
                | "School disambig"
                | "Human name dab"
                | "Template disambig"
                | "Template dab"
                | "Dis"
                | "Template ambiguous"
                | "Human name disambiguation"
                | "Number disambiguation"
                | "Place name disambiguation"
                | "Ship index"
                | "Surname"
                | "Road index"
                | "School disambiguation"
                | "Disambiguation"
                | "Hospital disambiguation"
                | "Mountain index"
                | "Given name"
                | "Mathematical disambiguation"
                | "Chinese title disambiguation"
                | "Airport disambiguation"
                | "Set index article"
                | "Dmbox"
                | "Sport index"
                | "Call sign disambiguation"
                | "Plant common name"
                | "Lake index"
                | "Molecular formula index"
                | "Species Latin name disambiguation"
                | "Letter-number combination disambiguation"
                | "Species Latin name abbreviation disambiguation"
                | "Genus disambiguation"
                | "Taxonomy disambiguation"
                | "Chemistry index"
                | "Biology disambiguation"
                | "Taxonomic authority disambiguation"
                | "Military unit disambiguation"
                | "Synagogue disambiguation"
                | "Road disambiguation"
                | "Enzyme index"
                | "Media set index"
                | "Caselaw disambiguation"
                | "Fungus common name"
                | "Phonetics disambiguation"
                | "Animal common name"
                | "Storm index"
                | "River index"
                | "Locomotive index"
                | "Nickname"
                | "Music disambiguation"
                | "Station disambiguation"
                | "Portal disambiguation"
                | "Template disambiguation"
                | "Opus number disambiguation"
                | "WoO number disambiguation"
        )
    } else {
        false
    }
}
