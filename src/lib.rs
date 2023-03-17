use error_chain::error_chain;
use pyo3::{exceptions::PyValueError, prelude::*};

mod dumps;
mod wikitext;

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
        WikitextParseError(m: String) {
            description("wikitext parse error"),
            display("wikitext parse error: {}", m)
        }
    }
}

/// Formats the sum of two numbers as string.
#[pyfunction]
fn load_bz2(dump_path: &str, output_path: &str) -> PyResult<()> {
    dumps::load(dump_path, output_path)
        .map_err(|err| PyValueError::new_err(err.to_string()))
}

#[pyfunction]
fn test_parser(text: &str) -> PyResult<usize> {
    let tokens = wikitext::tokenize(text)
        .map_err(|err| PyValueError::new_err(err.to_string()))?;
    Ok(tokens.len())
}

#[pyfunction]
fn get_links(text: &str) -> PyResult<Vec<String>> {
    wikitext::get_links(text)
        .map_err(|err| PyValueError::new_err(err.to_string()))
}

#[pyfunction]
fn get_cite_urls(text: &str) -> PyResult<Vec<String>> {
    wikitext::get_cite_urls(text)
        .map_err(|err| PyValueError::new_err(err.to_string()))
}

#[pyfunction]
fn is_disambiguation_page(text: &str) -> PyResult<bool> {
    wikitext::is_disambiguation_page(text)
        .map_err(|err| PyValueError::new_err(err.to_string()))
}

/// A Python module implemented in Rust.
#[pymodule]
fn wikiplain(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(load_bz2, m)?)?;
    m.add_function(wrap_pyfunction!(test_parser, m)?)?;
    m.add_function(wrap_pyfunction!(get_links, m)?)?;
    m.add_function(wrap_pyfunction!(get_cite_urls, m)?)?;
    m.add_function(wrap_pyfunction!(is_disambiguation_page, m)?)?;
    Ok(())
}
