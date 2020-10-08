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

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum ValueType {
    Integer,
    Float,
}

type Variables = HashMap<String, ValueType>;
type Params = Vec<(String, ValueType)>;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Language {
    Variable(String, ValueType),
    Number(String),
    Infix(Operation, Box<Language>, Box<Language>),
    Function(String, Params, Vec<ValueType>, Vec<Language>),
    Call(String, Vec<Language>, ValueType),
    Block(Vec<Language>),
    Module(String, Vec<Language>, Vec<Language>),
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
        Language::Variable(name, _) => {
            format!("(local.get ${})", name)
        },
        Language::Number(number) => {
            format!("(i32.const {})", number)
        },
        Language::Infix(operation, left, right) => {
            if &Operation::Eq == operation {
                if let Language::Variable(name, _) = left.as_ref() {
                    return format!("(local.set ${} {})", name, to_wasm(right))
                }
            }

            let method = match operation {
                Operation::Add => "add",
                Operation::Minus => "sub",
                Operation::Mult => "mul",
                Operation::Div => "div",
                Operation::Eq => "=",
            };

            format!("({}.{} {} {})", value_type_to_wasm(find_value_type(left)), method, to_wasm(left), to_wasm(right))
        },
        Language::Function(name, params, results, block) => {
            let params_str = params.into_iter()
                .map( |(name, value_type)| format!("(param ${} {})", name, value_type_to_wasm(value_type)) )
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

            let locals = find_locals(&block, &params).into_iter()
                .map( |(name, value_type)| format!("(local ${} {})", name, value_type_to_wasm(&value_type)) )
                .collect::<Vec<String>>()
                .join(" ");

            let body = vec![params_str.to_string(), results.to_string(), locals.to_string(), instructions.to_string()].into_iter()
                .filter( |x| x != "" )
                .collect::<Vec<String>>()
                .join(" ");

            format!("(func ${} {})", name, body)
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
        },
        Language::Module(name, functions, instructions) => {

            let functions_str = functions.into_iter()
                .map(|function| to_wasm(function) )
                .collect::<Vec<String>>()
                .join(" ");

            let main = if instructions.is_empty() {
                "".to_string()
            } else {
                let value_type = match instructions.last() {
                    Some(instruction) => find_value_type(instruction),
                    None => panic!("No instructions")
                };

                let main = Language::Function(
                    "main".to_string(),
                    vec![],
                    vec![value_type.clone()],
                    instructions.clone()
                );

                to_wasm(&main)
            };

            let body = vec![functions_str, main].into_iter()
                .filter( |x| x != "" )
                .collect::<Vec<String>>()
                .join(" ");

            format!("(module ${} {})", name, body)
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
        Language::Module(_, _, instructions) => {
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
                panic!("variable: ${} not found", name)
            }
        },
        Rule::variable_def => {
            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str().to_string();
            let kind = inner.next().unwrap().as_str();

            let value_type = str_to_value_type(kind);

            variables.insert(name.to_string(), value_type.clone());

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
        Rule::module => {
            let mut instructions = vec![];
            let mut functions = vec![];

            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str().to_string();

            let block = inner.next().unwrap();

            let mut pair = block.into_inner();

            while let Some(instruction) = pair.next() {
                let ast = build_ast(instruction, variables)?;

                match ast {
                    Language::Function(_, _, _, _) => functions.push(ast),
                    _ => instructions.push(ast)
                }
            }

            Ok(Language::Module(name, functions, instructions))
        }
        x => panic!("WTF: {:?}", x)
    }
}

pub fn to_ast(original: &str, variables: &mut Variables) -> Result<Language, Error<Rule>> {
    let mut block = vec![];

    match UniversalParser::parse(Rule::language, original) {
        Ok(pairs) => {
            let mut pair = pairs.into_iter();

            match pair.next() {
                Some(pair) => {
                    match build_ast(pair, variables) {
                        Ok(ast) => block.push(ast),
                        Err(e) => panic!(e)
                    }
                },
                None => ()
            }
        },
        Err(e) => return Err(e)
    }

    if block.len() == 1 {
        Ok(block[0].to_owned())
    } else {
        Ok(Language::Block(block))
    }
}

fn find_locals(instructions: &Vec<Language>, params: &Params) -> Variables {
    let mut variables: Variables = HashMap::new();

    for instruction in instructions.iter() {
        find_local(instruction, &mut variables, &params);
    }

    variables
}

fn find_local<'a>(instruction: &'a Language, variables: &'a mut Variables, params: &Params) -> &'a Variables {
    match instruction {
        Language::Variable(name, value_type) => {
            if !name_on_params(&name, params) {
                variables.insert(name.to_string(), value_type.clone());
            }
        },
        Language::Infix(_, left, right) => {
            find_local(left, variables, params);
            find_local(right, variables, params);
        }
        _ => ()
    }

    variables
}

fn name_on_params(name: &String, params: &Params) -> bool {
    for (params_name, _) in params.into_iter() {
        if name == params_name {
            return true
        }
    }

    false
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
