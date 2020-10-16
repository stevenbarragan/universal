mod wasm;
mod ast;

#[macro_use]
extern crate pest_derive;

use std::collections::HashMap;
use wasmer::{Store, Module, Instance, Value, imports, Function, WasmPtr, Array};
use wasmer_compiler_cranelift::Cranelift;
use wasmer_engine_jit::JIT;
use std::fs;
use std::str;
use wasm::{to_wasm, Data};

use ast::{to_ast};

pub fn execute_file(filename: &str) -> anyhow::Result<()> { let file = fs::read_to_string(filename) .expect("Something went wrong reading the file");

    execute(&format!("(module {}\n{})", filename, file))
}

pub fn compile(string: &str) -> anyhow::Result<String> {
    let mut variables = HashMap::new();
    let mut data: Data = Default::default();

    let ast = to_ast(string, &mut variables)?;

    Ok(to_wasm(&ast, &mut data))
}

pub fn execute(string: &str) -> anyhow::Result<()> {
    let wasm = compile(string)?;

    let compiler = Cranelift::default();
    let store = Store::new(&JIT::new(&compiler).engine());

    let module = Module::new(&store, &wasm)?;
    let import_object = imports! {};
    let instance = Instance::new(&module, &import_object)?;

    let main = instance.exports.get_function("main")?;
    let memory = instance.exports.get_memory("mem")?;
    let result = main.call(&[])?;

    println!("{:?}", result);

    unsafe {
        println!("{:?}", str::from_utf8(&memory.data_unchecked()[0..2]));
    }

    Ok(())
}
