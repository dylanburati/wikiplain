use std::io::Read;

use pyo3::{exceptions::PyValueError, prelude::*};

mod dumps;
mod error;
mod wikitext;

/// Formats the sum of two numbers as string.
#[pyfunction]
fn load_bz2(dump_path: &str, output_path: &str) -> PyResult<()> {
    let res =
        dumps::load(dump_path, output_path).map_err(|err| PyValueError::new_err(err.to_string()));
    res
}

#[pyfunction]
fn test_parser(text: &str) -> PyResult<usize> {
    let tokens = wikitext::tokenize(text)
        .map_err(|err| PyValueError::new_err(err.to_string()))?;
    Ok(tokens.len())
}

/// A Python module implemented in Rust.
#[pymodule]
fn wikiplain(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(load_bz2, m)?)?;
    m.add_function(wrap_pyfunction!(test_parser, m)?)?;
    Ok(())
}
