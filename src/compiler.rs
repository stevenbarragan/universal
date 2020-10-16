use std::collections::HashMap;

use crate::wasm::{to_wasm, Data};
use crate::ast::to_ast;

pub fn compile(string: &str) -> anyhow::Result<String> {
    let mut variables = HashMap::new();
    let mut data: Data = Default::default();

    let ast = to_ast(string, &mut variables)?;

    Ok(to_wasm(&ast, &mut data))
}
