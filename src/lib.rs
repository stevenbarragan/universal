pub mod compiler;
pub mod ast;
pub mod wasm;

use std::fs;
use std::str;
use std::path::Path;
use wat;

use anyhow::Result;
use wasmtime::*;
use wasmtime_wasi::{Wasi, WasiCtx};

use compiler::compile;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use ast::*;

pub fn execute_file(filepath: &str) -> anyhow::Result<()> {
    let file = load_file(filepath);

    execute(&file)
}

pub fn execute(string: &str) -> anyhow::Result<()> {
    let mut variables = ast::Variables::new();
    let mut data: wasm::Data = Default::default();
    let ast = to_ast(string, &mut variables)?;

    match ast {
        Language::Program(modules) => {
	    let engine = Engine::default();
	    let store = Store::new(&engine);

	    let mut linker = Linker::new(&store);
	    let wasi = Wasi::new(&store, WasiCtx::new(std::env::args())?);
	    wasi.add_to_linker(&mut linker)?;

            linker.func("std", "puts", |x: i32| { println!("{}", x); x})?;
            linker.func("std", "print", |x: i32| { print!("{}", x); x})?;

            let mut instances = vec![];

            for module in modules {
                match &module {
                    Language::Module(name, _functions, _instructions, _exports, _imports) => {
                        let wasm = wasm::to_wasm(&module, &mut data);

                        // std library is autoloaded
                        if name == "std" {
                            continue;
                        }

                        let binary = wat::parse_str(wasm)?;

                        let wasm_module = Module::from_binary(&engine, binary.as_ref())?;

                        let instance = linker.instantiate(&wasm_module)?;
                    
                        linker.instance(&name, &instance)?;

                        instances.push(instance);
                    },
                    instruction => ()
                }
            }

            if let Some(program) = instances.last() {
                program.get_func("main")
                    .ok_or(anyhow::format_err!("failed to find `main` function export"))?
                    .call(&[])?; 
            }

            Ok(())
        },
        other => Ok(())
    }
}
