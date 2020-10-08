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
    Operation,
    Params,
    ValueType,
    Variables,
    find_value_type,
    to_ast,
};

mod wasm;
use wasm::{to_wasm};

use wasmer_runtime::{
    instantiate,
    DynFunc,
    Value,
    imports,
    error,

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

    rl.set_helper(Some(validator));

    loop {
	let readline = rl.readline(">> ");

	match readline {
	    Ok(line) => {
                lines.push_str(&format!("{  }\n", line));

                println!("lines: {}", lines);

                let mut variables = HashMap::new();

                let ast = to_ast(&lines, &mut variables)?;

                let main = Language::Function(
                    "main".to_string(),
                    vec![],
                    vec![find_value_type(&ast).clone()],
                    vec![ast]
                    );

		let module_wat = format!("(module {} (export \"main\" (func $main)))", to_wasm(&main));

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

#[cfg(test)]
mod test {
    use super::*;
    use Language::*;

    #[test]
    fn constants() {
        let mut variables = HashMap::new();

        assert_eq!(to_ast("42", &mut variables), Ok(Number("42".to_string())));

        assert_eq!(to_ast("
                42
                ", &mut variables), Ok(Number("42".to_string())));
    }

    #[test]
    fn variables() {
        let mut variables = HashMap::new();

        assert_eq!(to_ast("x: Int", &mut variables), Ok(Variable("x".to_string(), ValueType::Integer)));

        variables.insert("x".to_string(), ValueType::Integer);

        assert_eq!(to_ast("x", &mut variables), Ok(Variable("x".to_string(), ValueType::Integer)));
    }

    #[test]
    fn function_call() {
        let mut variables = HashMap::new();

        variables.insert("add".to_string(), ValueType::Integer);

        assert_eq!(to_ast("add(1)", &mut variables), Ok(Call("add".to_string(), vec![Number("1".to_string())], ValueType::Integer)));
    }

    #[test]
    fn operations() {
        let mut variables = HashMap::new();

        assert_eq!(to_ast("5 + 2", &mut variables),
            Ok(Infix( Operation::Add, Box::new(Number("5".to_string())), Box::new(Number("2".to_string())))));

        variables.insert("x".to_string(), ValueType::Integer);

        assert_eq!(to_ast("x + 1", &mut variables),
            Ok(Infix(Operation::Add, Box::new(Variable ("x".to_string(), ValueType::Integer)), Box::new(Number("1".to_string())))));
    }

    #[test]
    fn assignations() {
        let mut variables = HashMap::new();

        assert_eq!(to_ast("x: Int = 1", &mut variables),
            Ok(Infix(Operation::Eq, Box::new(Variable ("x".to_string(), ValueType::Integer)), Box::new(Number("1".to_string())))));

        variables.insert("x".to_string(), ValueType::Integer);

        assert_eq!(to_ast("x = 1", &mut variables),
            Ok(Infix(Operation::Eq, Box::new(Variable ("x".to_string(), ValueType::Integer)), Box::new(Number("1".to_string())))));
    }

    #[test]
    fn functions() {
        let mut variables = HashMap::new();

        let result = to_ast(
        "fn hello: Int
            42
        end", &mut variables);

        let expected = Function(
            "hello".to_string(),
            vec![],
            vec![ValueType::Integer],
            vec![Number("42".to_string())]
        );

        assert_eq!(result, Ok(expected));

        let mut variables = HashMap::new();

        let result = to_ast(
        "fn hello(num: Int): Int
          42
        end", &mut variables);

        let expected = Function(
            "hello".to_string(),
            vec![("num".to_string(), ValueType::Integer)],
            vec![ValueType::Integer],
            vec![Number("42".to_string())]
        );

        assert_eq!(result, Ok(expected));

        let mut variables = HashMap::new();

        let result = to_ast(
        "fn hello(num: Int): Int
            num
        end", &mut variables);

        let expected = Function(
            "hello".to_string(),
            vec![("num".to_string(), ValueType::Integer)],
            vec![ValueType::Integer],
            vec![Variable("num".to_string(), ValueType::Integer)]
        );

        assert_eq!(result, Ok(expected));

        let mut variables = HashMap::new();

        let result = to_ast(
        "fn hello(num: Int): Int
            num + 2
        end", &mut variables);

        let expected = Function(
            "hello".to_string(),
            vec![("num".to_string(), ValueType::Integer)],
            vec![ValueType::Integer],
            vec![Infix(Operation::Add, Box::new(Variable ("num".to_string(), ValueType::Integer)), Box::new(Number("2".to_string())))]
        );

        assert_eq!(result, Ok(expected));

        let mut variables = HashMap::new();

        let result = to_ast(
        "fn add(num: Int, num2: Int): Int
            num + num2
        end", &mut variables);

        let expected = Function(
            "add".to_string(),
            vec![("num".to_string(), ValueType::Integer), ("num2".to_string(), ValueType::Integer)],
            vec![ValueType::Integer],
            vec![Infix(Operation::Add, Box::new(Variable ("num".to_string(), ValueType::Integer)), Box::new(Variable ("num2".to_string(), ValueType::Integer)))]
        );

        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn function_to_wasm() {
        let function = Function(
            "add2".to_string(),
            vec![("num".to_string(), ValueType::Integer)],
            vec![ValueType::Integer],
            vec![Infix(Operation::Add, Box::new(Variable ("num".to_string(), ValueType::Integer)), Box::new(Number("2".to_string())))]
        );

        assert_eq!(to_wasm(&function), "(func $add2 (param $num i32) (result i32) (i32.add (local.get $num) (i32.const 2)))");

        let mut variables = HashMap::new();

        let function_ast = to_ast(
        "fn tres: Int
            num: Int = 3
        end", &mut variables);

        match function_ast {
            Ok(function) =>  {
                assert_eq!(to_wasm(&function), "(func $tres (result i32) (local $num i32) (local.set $num (i32.const 3)))");
            },
            Err(e) => println!("{}", e),
        }

        let function_ast = to_ast(
        "fn tres(x: Int): Int
            num: Int = 3
            x + num
        end", &mut variables);

        match function_ast {
            Ok(function) =>  {
                assert_eq!(to_wasm(&function), 
                    "(func $tres (param $x i32) (result i32) (local $num i32) (local.set $num (i32.const 3)) (i32.add (local.get $x) (local.get $num)))");
            },
            Err(e) => println!("{}", e),
        }
    }

    #[test]
    fn assignation_to_wasm() {
        let assignation = Infix(Operation::Eq, Box::new(Variable ("x".to_string(), ValueType::Integer)), Box::new(Number("1".to_string())));

        assert_eq!(to_wasm(&assignation), "(local.set $x (i32.const 1))")
    }

    #[test]
    fn modules() {
        let mut variables = HashMap::new();

        let module = to_ast(
        "module awesome
            42
        end
        ", &mut variables);

        let expected  = Module("awesome".to_string(), vec![], vec![Number("42".to_string())]);

        let mut variables = HashMap::new();

        let module = to_ast(
        "module awesome
            fn tres(): Int
                3
            end
        end", &mut variables);

        let function = Function(
            "tres".to_string(),
            vec![],
            vec![ValueType::Integer],
            vec![Language::Number("3".to_string())]
        );

        let expected  = Language::Module("awesome".to_string(), vec![function], vec![]);

        assert_eq!(module, Ok(expected));
        
        let function = Function(
            "tres".to_string(),
            vec![],
            vec![ValueType::Integer],
            vec![Language::Number("3".to_string())]
        );

        let module = Language::Module("awesome".to_string(), vec![function], vec![Number("42".to_string())]);

        let expected = "(module $awesome (func $tres (result i32) (i32.const 3)) (func $main (result i32) (i32.const 42)))";

        assert_eq!(to_wasm(&module), expected);
    }
}
