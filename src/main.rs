extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "universal.pest"]
pub struct UniversalParser;

extern crate wat;


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
        use ValidationResult::{Incomplete, Invalid, Valid};
        let input = ctx.input();

	let result = if let Ok(_) = UniversalParser::parse(Rule::language, input) {
            Valid(None)
        } else {
            Incomplete
        };

        Ok(result)
    }
}

fn main() -> anyhow::Result<()> {
    let validator = InputValidator {};
    let mut rl = Editor::new();

    rl.set_helper(Some(validator));

    loop {
	let readline = rl.readline(">> ");

	match readline {
	    Ok(line) => {
                let mut variables = HashMap::new();

                let ast = to_ast(&line, &mut variables)?;

		let module_wat = format!(
                    "(module (func (export \"main\") (result {}) {}))",
                    value_type_to_wasm(find_value_type(&ast)),
                    to_wasm(&ast)
                );

                println!("{:?}", module_wat);

                let binary = wat::parse_str(module_wat)?;

                let import_object = imports! {};

                if let Ok(instance) = instantiate(&binary, &import_object) {
                    let main = instance
                        .exports
                        .get::<DynFunc>("main")?;

                    match main.call(&[]) {
                        Ok(result) => println!("=> {:?}", result),
                        e => println!("{:?}", e)
                    }
                };
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

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum ValueType {
    Integer,
    Float,
}

type Variables = HashMap<String, ValueType>;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Language {
    Variable(String, ValueType),
    Number(String),
    Infix(Operation, Box<Language>, Box<Language>),
    Function(String, Vec<(String, ValueType)>, Vec<ValueType>, Vec<Language>),
    Call(String, Vec<Language>, ValueType),
    Block(Vec<Language>),
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Operation {
    Add,Minus,Mult,Div,Eq
}

use pest::error::Error;
use pest::iterators::Pair;

fn str_to_value_type(value_type: &str) -> ValueType {
    match value_type {
        "Int" => ValueType::Integer,
        "Float" => ValueType::Float,
        _ => panic!("Value type undefined")
    }
}

fn to_wasm(node: &Language) -> String {
    match node {
        Language::Variable(name, value_type) => {
            format!("(local ${} {})", name, value_type_to_wasm(value_type))
        },
        Language::Number(number) => {
            format!("(i32.const {})", number)
        },
        Language::Infix(operation, left, right) => {
            let method = match operation {
                Operation::Add => "add",
                Operation::Minus => "sub",
                Operation::Mult => "mul",
                Operation::Div => "div",
                _ => panic!("operation no suported yet")
            };

            format!("({}.{} {} {})", value_type_to_wasm(find_value_type(left)), method, to_wasm(left), to_wasm(right))
        },
        Language::Function(name, params, results, block) => {
            let params = params.into_iter()
                .map( |(name, value_type)| format!("(param {} {})", name, value_type_to_wasm(value_type)) )
                .collect::<Vec<String>>()
                .join(" ");

            let results = results.into_iter()
                .map( |value_type| format!("(result {})", value_type_to_wasm(value_type)))
                .collect::<Vec<String>>()
                .join(" ");

            let instructions = block.into_iter()
                .map( |language| to_wasm(language) )
                .collect::<Vec<String>>()
                .join(" ");

            format!("(${} {} {} {})", name, params, results, instructions)
        },
        Language::Call(function_name, params, _) => {
            let params = params.into_iter()
                .map( |language| to_wasm(language) )
                .collect::<Vec<String>>()
                .join(" ");

            format!("call(${}, {})", function_name, params)
        },
        Language::Block(instructions) => {
            instructions.into_iter()
                .map( |language| to_wasm(language) )
                .collect::<Vec<String>>()
                .join(" ")
        }
    }
}

fn find_value_type(node: &Language) -> &ValueType {
    match node {
        Language::Variable(_, value_type) => value_type,
        Language::Number(_) => &ValueType::Integer,
        Language::Infix(operation, left, right) => {
            find_value_type(left)
        },
        Language::Function(_, _, results, _) => {
            &results[0]
        }
        Language::Call(_, _, value_type) => value_type,
        Language::Block(instructions) => {
            if let Some(instruction) = instructions.last() {
                find_value_type(instruction) 
            } else {
                panic!("No instructions")
            }
        }
    }
}

fn value_type_to_wasm(value_type: &ValueType) -> String {
    match value_type {
        ValueType::Integer => "i32".to_string(),
        ValueType::Float => "f32".to_string(),
    }
}

fn build_ast(pair: Pair<Rule>, variables: &mut Variables) -> Result<Language, Error<Rule>> {
    match pair.as_rule() {
        Rule::integer => {
            let value = pair.as_str().to_string();

            Ok(Language::Number(value))
        },
        Rule::variable => {
            let name = pair.as_str().to_string();

            if let Some(kind) = variables.get(&name) {
                Ok(Language::Variable(name, kind.clone()))
            } else {
                panic!("No variable found")
            }
        },
        Rule::variable_def => {
            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str().to_string();
            let kind = inner.next().unwrap().as_str();

            let value_type = str_to_value_type(kind);

            Ok(Language::Variable(name, value_type))
        },
        Rule::infix => {
            let mut inner = pair.into_inner();
            let left = build_ast(inner.next().unwrap(), variables)?;
            let message = inner.next().unwrap().as_str();
            let right = build_ast(inner.next().unwrap(), variables)?;

            let operation = match message {
                "+" => Operation::Add,
                "-" => Operation::Minus,
                "*" => Operation::Mult,
                "/" => Operation::Div,
                "=" => Operation::Eq,
                _ => panic!("Operation expected")
            };

            Ok(Language::Infix(operation, Box::new(left), Box::new(right)))
        },
        Rule::function_def => {
            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str().to_string();

            let mut results = vec![];
            let mut params = vec![];
            let mut instructions = vec![];

            let mut elem = inner.next().unwrap();

            if elem.as_rule() == Rule::params {
                let mut params_inner = elem.into_inner();

                while let Some(param) = params_inner.next() {
                    let mut pair = param.into_inner();

                    let name = pair.next().unwrap().as_str();
                    let kind_str = pair.next().unwrap().as_str();

                    let kind = str_to_value_type(kind_str);

                    params.push((name.to_string(), kind.clone()));

                    variables.insert(name.to_string(), kind.clone());
                }

                elem = inner.next().unwrap();
            }

            if elem.as_rule() == Rule::results {
                let mut pair = elem.into_inner();

                while let Some(rule) = pair.next() {
                    let kind = str_to_value_type(rule.as_str());

                    variables.insert(name.to_string(), kind.clone());

                    results.push(kind);
                }

                elem = inner.next().unwrap();
            }

            if elem.as_rule() == Rule::block {
                let mut pair = elem.into_inner();

                while let Some(instruction) = pair.next() {
                    instructions.push(build_ast(instruction, variables)?);
                }
            }

            Ok(Language::Function(name, params, results, instructions))
        },
        Rule::function_call => {
            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str().to_string();

            let mut params = vec![];

            let mut params_inner = inner.next().unwrap().into_inner();

            while let Some(param) = params_inner.next() {
                params.push(build_ast(param, variables)?);
            }

            if let Some(value_type) = variables.get(&name) {
                Ok(Language::Call(name, params, value_type.clone()))
            } else {
                panic!("No variable found")
            }
        },
        _ => panic!("WTF")
    }
}

pub fn to_ast(original: &str, variables: &mut Variables) -> Result<Language, Error<Rule>> {
    let pair = UniversalParser::parse(Rule::language, original)?.next().unwrap();

    build_ast(pair, variables)
}

#[cfg(test)]
mod test {
    use super::*;
    use Language::*;

    #[test]
    fn constants() {
        let mut variables = HashMap::new();

        assert_eq!(to_ast("42", &mut variables), Ok(Number("42".to_string())));
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

        assert_eq!(to_wasm(&function), "($add2 (param num i32) (result i32) (i32.add (local $num i32) (i32.const 2)))")
    }
}
