pub mod compiler;
pub mod ast;
pub mod wasm;

use wasmer::{Store, Module, Instance, Value, imports, Function, WasmPtr, Array};
use wasmer_compiler_cranelift::Cranelift;
use wasmer_engine_jit::JIT;
use std::fs;
use std::str;

use compiler::compile;

extern crate pest;
#[macro_use]
extern crate pest_derive;

pub fn execute_file(filename: &str) -> anyhow::Result<()> {
    let file = fs::read_to_string(filename).expect("Something went wrong reading the file");

    execute(&format!("module awesome\n{} end", file))
}

pub fn execute(string: &str) -> anyhow::Result<()> {
    let wasm = compile(string)?;

    println!("{:?}", wasm);

    let compiler = Cranelift::default();
    let store = Store::new(&JIT::new(&compiler).engine());

    let module = Module::new(&store, &wasm)?;
    let import_object = imports! {};
    let instance = Instance::new(&module, &import_object)?;

    let main = instance.exports.get_function("main")?;
    let result = main.call(&[])?;

    println!("{:?}", result);

    // let memory = instance.exports.get_memory("mem")?;
    // unsafe {
    //     println!("{:?}", str::from_utf8(&memory.data_unchecked()[0..2]));
    // }

    Ok(())
}
