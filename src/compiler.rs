use std::fs;

use crate::ast::to_ast;
use crate::wasm::{to_wasm, Data};

pub fn compile(string: &str) -> anyhow::Result<String> {
    let mut data: Data = Default::default();

    let ast = to_ast(string)?;

    Ok(to_wasm(&ast, &mut data))
}

pub fn compile_from_file(file_path: &str) -> anyhow::Result<String> {
    let file = fs::read_to_string(file_path).expect("Something went wrong reading the file");

    compile(&file)
}
