use std::collections::HashMap;

use crate::ast::{
    Language,
    Operation,
    Params,
    ValueType,
    Variables,
    find_value_type,
};

pub fn to_wasm(node: &Language) -> String {
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

fn value_type_to_wasm(value_type: &ValueType) -> String {
    match value_type {
        ValueType::Integer => "i32".to_string(),
        ValueType::Float => "f32".to_string(),
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
