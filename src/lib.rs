use std::fmt::Debug;
use std::ops::Deref;

use error_chain::error_chain;
use pyo3::{exceptions::PyValueError, prelude::*, PyRef, wrap_pyfunction};

mod dumps;
mod wikitext;
mod wikitext_ast;

use tantivy::tokenizer::{TextAnalyzer, SimpleTokenizer, LowerCaser, TokenStream, AsciiFoldingFilter};
use wikitext::Token;
use wikitext_ast::Node;

error_chain! {
    foreign_links {
        IoError(std::io::Error);
        ParseIntError(std::num::ParseIntError);
        ParquetError(parquet::errors::ParquetError);
        XmlError(quick_xml::Error);
    }

    errors {
        InvalidArgument(m: String) {
            description("invalid argument"),
            display("invalid argument: {}", m)
        }
        InvalidIndexFile {
            description("invalid index file"),
        }
        SQLParseError(m: &'static str) {
            description("SQL parse error"),
            display("SQL parse error: {}", m)
        }
        WikitextParseError(m: String) {
            description("wikitext parse error"),
            display("wikitext parse error: {}", m)
        }
        ASTParseError(m: String, pos: usize) {
            description("AST parse error"),
            display("AST parse error at {}: {}", pos, m)
        }
        ConvertError(m: String) {
            description("conversion error"),
            display("conversion error: {}", m)
        }
    }
}

/// Load a Wikimedia XML dump into a parquet file.
#[pyfunction]
fn load_bz2(dump_path: &str, output_path: &str) -> PyResult<()> {
    dumps::load_pages_articles(dump_path, output_path).map_err(|err| PyValueError::new_err(err.to_string()))
}

/// Parse a SQL insert statement and get the table name and list of rows to insert.
#[pyfunction(name = "parse_insert_statement")]
fn parse_sql_insert_statement(text: &[u8]) -> PyResult<(String, Vec<Vec<dumps::SqlLiteral>>)> {
    dumps::parse_sql_insert_statement(text).map_err(|err| PyValueError::new_err(err.to_string()))
}

/// Test the Wikitext tokenizer.
#[pyfunction]
fn test_parser(text: &str) -> PyResult<usize> {
    let tokens = wikitext::tokenize(text).map_err(|err| PyValueError::new_err(err.to_string()))?;
    Ok(tokens.len())
}

#[pyclass(name = "TokenKind")]
#[derive(Clone, Debug)]
enum PyTokenKind {
    Comment,
    TemplateStart,
    TemplateEnd,
    LinkStart,
    LinkEnd,
    ElementStart,
    ElementStartEnd,
    ElementEnd,
    Content,
}

#[pyclass(name = "Token")]
#[derive(Debug)]
struct PyToken {
    #[pyo3(get)]
    kind: PyTokenKind,
    #[pyo3(get)]
    data: Option<String>,
}

#[pymethods]
impl PyToken {
    #[new]
    fn new(kind: PyTokenKind, data: Option<String>) -> Self {
        PyToken { kind, data }
    }

    fn __repr__(&self) -> String {
        format!("({}, {:?})", self.kind.__pyo3__repr__(), self.data)
    }
}

impl<'a> From<Token<'a>> for PyToken {
    fn from(value: Token<'a>) -> Self {
        match value {
            Token::Comment => PyToken::new(PyTokenKind::Comment, None),
            Token::TemplateStart => PyToken::new(PyTokenKind::TemplateStart, None),
            Token::TemplateEnd => PyToken::new(PyTokenKind::TemplateEnd, None),
            Token::LinkStart => PyToken::new(PyTokenKind::LinkStart, None),
            Token::LinkEnd => PyToken::new(PyTokenKind::LinkEnd, None),
            Token::ElementStart(name, false) => PyToken::new(PyTokenKind::ElementStart, Some(name)),
            Token::ElementStart(name, true) => {
                PyToken::new(PyTokenKind::ElementStartEnd, Some(name))
            }
            Token::ElementEnd(name) => PyToken::new(PyTokenKind::ElementEnd, Some(name)),
            Token::Content(text) => PyToken::new(PyTokenKind::Content, Some(text.to_owned())),
        }
    }
}

impl<'a> TryFrom<&'a PyToken> for Token<'a> {
    type Error = Error;

    fn try_from(value: &'a PyToken) -> Result<Self> {
        let e = || Error::from(ErrorKind::ConvertError(format!("{:?}", value)));
        match value.kind {
            PyTokenKind::Comment => Ok(Token::Comment),
            PyTokenKind::TemplateStart => Ok(Token::TemplateStart),
            PyTokenKind::TemplateEnd => Ok(Token::TemplateEnd),
            PyTokenKind::LinkStart => Ok(Token::LinkStart),
            PyTokenKind::LinkEnd => Ok(Token::LinkEnd),
            PyTokenKind::ElementStart => {
                let name = value.data.as_ref().ok_or_else(e)?;
                Ok(Token::ElementStart(name.clone(), false))
            }
            PyTokenKind::ElementStartEnd => {
                let name = value.data.as_ref().ok_or_else(e)?;
                Ok(Token::ElementStart(name.clone(), true))
            }
            PyTokenKind::ElementEnd => {
                let name = value.data.as_ref().ok_or_else(e)?;
                Ok(Token::ElementEnd(name.clone()))
            }
            PyTokenKind::Content => {
                let text = value.data.as_ref().ok_or_else(e)?;
                Ok(Token::Content(text.as_str()))
            }
        }
    }
}

/// Tokenize Wikitext.
#[pyfunction]
fn tokenize(text: &str) -> PyResult<Vec<PyToken>> {
    wikitext::tokenize(text)
        .map_err(|err| PyValueError::new_err(err.to_string()))
        .map(|tokens| tokens.into_iter().map(PyToken::from).collect())
}

#[pyclass(name = "NodeKind")]
#[derive(Clone, Debug)]
enum PyNodeKind {
    Document,
    Element,
    Template,
    Link,
    Argument,
    Content,
}

#[pyclass(name = "Node")]
#[derive(Clone)]
struct PyNode {
    #[pyo3(get)]
    kind: PyNodeKind,
    #[pyo3(get)]
    children: Vec<PyNode>,
    #[pyo3(get)]
    data: Option<String>,
}

impl Debug for PyNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            PyNodeKind::Document => f.debug_tuple("Document").field(&self.children).finish(),
            PyNodeKind::Element => {
                let name = self.data.clone().unwrap_or("".to_string());
                f.debug_tuple("Element")
                    .field(&name)
                    .field(&self.children)
                    .finish()
            }
            PyNodeKind::Template => f.debug_tuple("Template").field(&self.children).finish(),
            PyNodeKind::Link => f.debug_tuple("Link").field(&self.children).finish(),
            PyNodeKind::Argument => f.debug_tuple("Argument").field(&self.children).finish(),
            PyNodeKind::Content => {
                let text = self.data.clone().unwrap_or("".to_string());
                f.debug_tuple("Content").field(&text).finish()
            }
        }
    }
}

#[pymethods]
impl PyNode {
    #[new]
    fn new(kind: PyNodeKind, children: Vec<PyNode>, data: Option<String>) -> Self {
        PyNode {
            kind,
            children,
            data,
        }
    }

    fn __repr__(&self) -> String {
        format!("{:?}", self)
    }
}

impl From<Node> for PyNode {
    fn from(value: Node) -> Self {
        match value {
            Node::Document(children) => PyNode::new(
                PyNodeKind::Document,
                children.into_iter().map(PyNode::from).collect(),
                None,
            ),
            Node::Element(name, children) => PyNode::new(
                PyNodeKind::Element,
                children.into_iter().map(PyNode::from).collect(),
                Some(name),
            ),
            Node::Template(children) => PyNode::new(
                PyNodeKind::Template,
                children.into_iter().map(PyNode::from).collect(),
                None,
            ),
            Node::Link(children) => PyNode::new(
                PyNodeKind::Link,
                children.into_iter().map(PyNode::from).collect(),
                None,
            ),
            Node::Argument(children) => PyNode::new(
                PyNodeKind::Argument,
                children.into_iter().map(PyNode::from).collect(),
                None,
            ),
            Node::Content(text) => PyNode::new(PyNodeKind::Content, vec![], Some(text)),
        }
    }
}

/// Build a syntax tree from tokenized Wikitext.
#[pyfunction]
fn parse_tokens(tokens: Vec<PyRef<PyToken>>) -> PyResult<PyNode> {
    let mut native_tokens = vec![];
    for res in tokens.iter().map(|rt| rt.deref().try_into()) {
        native_tokens.push(res.map_err(|err: Error| PyValueError::new_err(err.to_string()))?);
    }
    wikitext_ast::parse(native_tokens)
        .map_err(|err| PyValueError::new_err(err.to_string()))
        .map(|doc| doc.into())
}

//lazy_static! {
    //static ref NLP_TOKENIZER: BoxTokenStream<'static> = TextAnalyzer::builder(SimpleTokenizer::default())
        //.filter(LowerCaser)
        //.build();
//}

/// Tokenize each English phrase as a space-separated term
#[pyfunction]
fn nlp_tokenize(text: Vec<String>) -> PyResult<Vec<String>> {
    let mut nlp = TextAnalyzer::builder(SimpleTokenizer::default())
        .filter(AsciiFoldingFilter)
        .filter(LowerCaser)
        .build();
    let mut result = Vec::with_capacity(text.len());
    for phrase in text.iter() {
        let mut stream = nlp.token_stream(phrase);
        let mut term = String::with_capacity(phrase.len());
        while let Some(t) = stream.next() {
            if !matches!(t.text.as_str(), "(" | "[" | "{" | "<" | "\"" | "'" | "«" | "‘" | "‚" | "‛" | "“" | "„" | "‟" | "‹" | "❮" | "``") {
                term.push_str(t.text.as_str());
                break;
            }
        }
        while let Some(t) = stream.next() {
            if !matches!(t.text.as_str(), "(" | "[" | "{" | "<" | "\"" | "'" | "«" | "‘" | "‚" | "‛" | "“" | "„" | "‟" | "‹" | "❮" | "``") {
                term.push(' ');
                term.push_str(t.text.as_str());
            }
        }
        result.push(term);
    }
    Ok(result)
}

/// Tokenize Wikitext and build a syntax tree.
#[pyfunction]
fn parse(text: &str) -> PyResult<PyNode> {
    let tokens = wikitext::tokenize(text).map_err(|err| PyValueError::new_err(err.to_string()))?;
    wikitext_ast::parse(tokens)
        .map_err(|err| PyValueError::new_err(err.to_string()))
        .map(|doc| doc.into())
}

/// Extract the internal links from a Wikitext string.
#[pyfunction]
fn get_links(text: &str) -> PyResult<Vec<String>> {
    wikitext::get_links(text).map_err(|err| PyValueError::new_err(err.to_string()))
}

/// Extract the URLs used in web citations from a Wikitext string.
#[pyfunction]
fn get_cite_urls(text: &str) -> PyResult<Vec<String>> {
    wikitext::get_cite_urls(text).map_err(|err| PyValueError::new_err(err.to_string()))
}

/// Detect whether the Wikitext string has a disambiguation marker template.
#[pyfunction]
fn is_disambiguation_page(text: &str) -> PyResult<bool> {
    wikitext::is_disambiguation_page(text).map_err(|err| PyValueError::new_err(err.to_string()))
}

/// Get the full template content for the "For other uses ..." or "This page is about _, ..." notes
/// on the page.
#[pyfunction]
fn get_distinguish_hatnotes(text: &str) -> PyResult<Vec<String>> {
    wikitext::get_distinguish_hatnotes(text).map_err(|err| PyValueError::new_err(err.to_string()))
}

/// Get the title of the first infobox template in the Wikitext, or None if not found.
#[pyfunction]
fn get_first_infobox_title(text: &str) -> PyResult<Option<String>> {
    wikitext::get_first_infobox_title(text).map_err(|err| PyValueError::new_err(err.to_string()))
}

fn sql_module(py: Python, parent_module: &PyModule) -> PyResult<()> {
    let m = PyModule::new(py, "sql")?;
    m.add_function(wrap_pyfunction!(parse_sql_insert_statement, m)?)?;
    parent_module.add_submodule(m)?;
    Ok(())
}

/// A Python module implemented in Rust.
#[pymodule]
fn wikiplain(py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<PyTokenKind>()?;
    m.add_class::<PyToken>()?;
    m.add_class::<PyNodeKind>()?;
    m.add_class::<PyNode>()?;
    m.add_function(wrap_pyfunction!(load_bz2, m)?)?;
    m.add_function(wrap_pyfunction!(test_parser, m)?)?;
    m.add_function(wrap_pyfunction!(nlp_tokenize, m)?)?;
    m.add_function(wrap_pyfunction!(tokenize, m)?)?;
    m.add_function(wrap_pyfunction!(parse_tokens, m)?)?;
    m.add_function(wrap_pyfunction!(parse, m)?)?;
    m.add_function(wrap_pyfunction!(get_links, m)?)?;
    m.add_function(wrap_pyfunction!(get_cite_urls, m)?)?;
    m.add_function(wrap_pyfunction!(is_disambiguation_page, m)?)?;
    m.add_function(wrap_pyfunction!(get_distinguish_hatnotes, m)?)?;
    m.add_function(wrap_pyfunction!(get_first_infobox_title, m)?)?;
    sql_module(py, m)?;
    Ok(())
}
