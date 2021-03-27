pub mod ast;
pub mod compiler;
pub mod wasm;

use std::fs;
use std::path::Path;
use std::str;
use wat;

use anyhow::Result;
use wasmtime::*;
use wasmtime_wasi::{Wasi};
use wasi_cap_std_sync::WasiCtxBuilder;

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
    let mut data: wasm::Data = Default::default();
    let ast = to_ast(string)?;

    match ast {
        Language::Program(modules) => {
            let engine = Engine::default();
            let store = Store::new(&engine);

            let memory_ty = MemoryType::new(Limits::new(1, None));
            let memory = Memory::new(&store, memory_ty);

            let tinyalloc_module = Module::from_file(&engine, "tinyalloc.wast")?;

            let mut linker = Linker::new(&store);
            let wasi = Wasi::new(
                &store,
                WasiCtxBuilder::new()
                .inherit_stdio()
                .inherit_args()?
                .build()?,
            );
            wasi.add_to_linker(&mut linker)?;

            linker.define("env", "memory", memory)?;

            let ty = GlobalType::new(ValType::I32, Mutability::Const);
            let memory_base = Global::new(&store, ty, Val::I32(0))?;
            linker.define("env", "__memory_base", memory_base)?;

            let tinyalloc_instance = linker.instantiate(&tinyalloc_module)?;
            linker.instance("tinyalloc", &tinyalloc_instance)?;

            linker.func("std", "puts_int", |x: i32| {
                println!("{}", x);
                x
            })?;

            linker.func("std", "print_int", |x: i32| {
                print!("{}", x);
                x
            })?;

            linker.func("std", "calloc_int_int", |x: i32, y: i32| {
                0
            })?;

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
                    }
                    instruction => (),
                }
            }

            if let Some(program) = instances.last() {
                program
                    .get_func("main")
                    .ok_or(anyhow::format_err!("failed to find `main` function export"))?
                    .call(&[])?;
            }

            Ok(())
        }
        other => Ok(()),
    }
}
