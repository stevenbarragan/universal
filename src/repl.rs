extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "universal.pest"]
pub struct UniversalParser;

extern crate wat;

mod ast;
use ast::{
    Language,
    ValueType,
    find_value_type,
    to_ast,
};

mod wasm;
use wasm::{to_wasm, Data};

use wasmer_runtime::{
    instantiate,
    DynFunc,
    imports,
};

use rustyline::error::ReadlineError;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::Editor;
use rustyline_derive::{Completer, Helper, Highlighter, Hinter};

#[derive(Completer, Helper, Highlighter, Hinter)]
struct InputValidator {}

impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext) -> Result<ValidationResult, ReadlineError> {
        use ValidationResult::{Incomplete, Valid};
        let input = ctx.input();

        match UniversalParser::parse(Rule::language, input) {
            Ok(_) => Ok(Valid(None)),
            _ => Ok(Incomplete)
        }
    }
}
fn main() -> anyhow::Result<()> {
    let validator = InputValidator {};
    let mut rl = Editor::new();
    let mut lines = String::new();
    let mut data: Data = Default::default();

    rl.set_helper(Some(validator));

    loop {
	let readline = rl.readline(">> ");

	match readline {
	    Ok(line) => {
                lines.push_str(&format!("{  }\n", line));

                let module = format!("module comandline {} end", lines);

                let mut variables = HashMap::new();

                let ast = to_ast(&module, &mut variables)?;

		let module_wat = to_wasm(&ast, &mut data);

                println!("{:?}", module_wat);

                let binary = wat::parse_str(module_wat)?;

                let import_object = imports! {};

                match instantiate(&binary, &import_object) {
                    Ok(instance) => {
                        let main = instance.exports.get::<DynFunc>("main")?;

                        match main.call(&[]) {
                            Ok(result) => println!("=> {:?}", result),
                            e => println!("{:?}", e)
                        }
                    },
                    Err(e) => println!("Err: {:?}", e)
                }
	    },
            Err(ReadlineError::Interrupted) => {
		println!("CTRL-C");
		break
	    },
	    Err(ReadlineError::Eof) => {
		println!("CTRL-D");
		break
	    },
	    Err(err) => {
		println!("Error: {:?}", err);
		break
	    }
	}

    }

    return Ok(())
}
