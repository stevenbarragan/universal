pub mod compiler;
pub mod ast;
pub mod wasm;

use std::fs;
use std::str;
use wat;

use anyhow::Result;
use wasmtime::*;
use wasmtime_wasi::{Wasi, WasiCtx};

use compiler::compile;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use ast::*;

pub fn execute_file(filename: &str) -> anyhow::Result<()> {
    let file = fs::read_to_string(filename).expect("Something went wrong reading the file");

    execute(&format!("module awesome\n{} end", file))
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

            let mut instances = vec![];

            for module in modules {
                let binary = wat::parse_str(wasm::to_wasm(&module, &mut data))?;

                match module {
                    Language::Module(name, _functions, _instructions, _exports, _imports) => {
                        let wasm_module = Module::from_binary(&engine, binary.as_ref())?;

                        let instance = linker.instantiate(&wasm_module)?;
                    
                        linker.instance(&name, &instance)?;

                        instances.push(instance);
                    },
                    instruction => ()
                }
            }

            if let Some(program) = instances.last() {
                let main = program
                    .get_func("main")
                    .ok_or(anyhow::format_err!("failed to find `run` function export"))?
                    .get0::<i32>()?;

                let result = main()?;

                println!("result: {:?}", result);
            }

            Ok(())
        },
        other => Ok(())
    }
}
