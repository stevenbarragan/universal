use std::collections::HashMap;
use crate::ast::*;

#[derive(Default)]
pub struct Data {
    pointers: HashMap<String, (usize, usize)>,
    memory: String
}

pub fn to_wasm(node: &Language, data: &mut Data) -> String {
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
                    return format!("(local.set ${} {})", name, to_wasm(right, data))
                }
            }

            let method = match operation {
                Operation::Add => "add",
                Operation::Minus => "sub",
                Operation::Mult => "mul",
                Operation::Div => "div",
                Operation::Eq => "=",
            };

            format!("({}.{} {} {})", value_type_to_wasm(find_value_type(left)), method, to_wasm(left, data), to_wasm(right, data))
        },
        Language::Function(name, params, results, block) => {
            let params_str = params.into_iter()
                .map( |(name, value_type)| format!("(param ${} {})", name, value_type_to_wasm(value_type)) )
                .collect::<Vec<String>>()
                .join(" ");

            let results = results.into_iter() .map( |value_type| format!("(result {})", value_type_to_wasm(value_type)))
                .collect::<Vec<String>>()
                .join(" ");

            let instructions = block.into_iter()
                .map( |language| to_wasm(language, data) )
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
        Language::Symbol(string) => {
            if let Some(position) = data.pointers.get(string) {
                format!("(i32.const {}) (i32.const {})", position.0, position.1)
            } else {
                let position = (data.memory.len(), string.len());

                data.pointers.insert(string.to_owned(), position);

                data.memory += string;

                format!("(i32.const {}) (i32.const {})", position.0, position.1)
            }
        },
        Language::Call(function_name, params, _) => {
            let params = params.into_iter()
                .map( |language| to_wasm(language, data) )
                .collect::<Vec<String>>()
                .join(" ");

            format!("call(${}, {})", function_name, params)
        },
        Language::Block(instructions) => {
            instructions.into_iter()
                .map( |language| to_wasm(language, data) )
                .collect::<Vec<String>>()
                .join(" ")
        },
        Language::Module(name, functions, instructions) => {
            let functions_str = functions.into_iter()
                .map( |language| to_wasm(language, data) )
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

                to_wasm(&main, data)
            };

            let data_str = if data.memory.len() > 0 {
                format!("(memory (export \"mem\") 1) (data (i32.const 0) \"{}\")", data.memory)
            } else {
                "".to_string()
            };

            let body = vec![data_str, functions_str, main].into_iter()
                .filter( |x| x != "" )
                .collect::<Vec<String>>()
                .join(" ");

            format!("(module ${} {})", name, body)
        },
        Language::Conditional(conditional_type, instruction, block, block2) => {
            let conditional = to_wasm(instruction, data);
            let block = to_wasm(block, data);

            match conditional_type {
                ConditionalType::If => match block2 {
                    Some(x) => format!("(if {} (then {}) (else {}))", conditional, block, to_wasm(x, data)),
                    None => format!("(if {} (then {}))", conditional, block)
                }
                ConditionalType::Unless => match block2 {
                    Some(x) => format!("(if (i32.eqz {}) (then {}) (else {}))", conditional, block, to_wasm(x, data)),
                    None => format!("(if (i32.eqz {}) (then {}))", conditional, block)
                }
            }
        }
    }
}

fn value_type_to_wasm(value_type: &ValueType) -> String {
    match value_type {
        ValueType::Integer => "i32".to_string(),
        ValueType::Float => "f32".to_string(),
        ValueType::Symbol => "i32 i32".to_string(),
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
    fn symbols() {
        let mut data: Data = Default::default();

        let module = Language::Module("awesome".to_string(), vec![], vec![Symbol("42".to_string())]);

        let expected = "(module $awesome (memory (export \"mem\") 1) (data (i32.const 0) \"42\") (func $main (result i32 i32) (i32.const 0) (i32.const 2)))";

        assert_eq!(to_wasm(&module, &mut data), expected);

        let mut data: Data = Default::default();

        let module = Language::Module("awesome".to_string(), vec![], vec![Symbol("42".to_string()), Symbol("43".to_string())]);

        let expected = "(module $awesome (memory (export \"mem\") 1) (data (i32.const 0) \"4243\") (func $main (result i32 i32) (i32.const 0) (i32.const 2) (i32.const 2) (i32.const 2)))";

        assert_eq!(to_wasm(&module, &mut data), expected)
    }

    #[test]
    fn conditional_if() {
        let instructions = vec![Number("1".to_string())];

        let conditional = Conditional(
            ConditionalType::If,
            Box::new(Number("1".to_string())),
            Box::new(Language::Block(instructions)),
            None
        );

        let expected = "(if (i32.const 1) (then (i32.const 1)))";

        let mut data: Data = Default::default();

        assert_eq!(to_wasm(&conditional, &mut data), expected);
    }

    #[test]
    fn conditional_if_else() {
        let instructions = [Number("1".to_string())];

        let conditional = Conditional(
            ConditionalType::If,
            Box::new(Number("1".to_string())),
            Box::new(Language::Block(vec![Number("1".to_string())])),
            Some(Box::new(Language::Block(vec![Number("2".to_string())]))),
        );

        let expected = "(if (i32.const 1) (then (i32.const 1)) (else (i32.const 2)))";

        let mut data: Data = Default::default();

        assert_eq!(to_wasm(&conditional, &mut data), expected);
    }

    #[test]
    fn conditional_unless() {
        let instructions = vec![Number("1".to_string())];

        let conditional = Conditional(
            ConditionalType::Unless,
            Box::new(Number("1".to_string())),
            Box::new(Language::Block(instructions)),
            None
        );

        let expected = "(if (i32.eqz (i32.const 1)) (then (i32.const 1)))";

        let mut data: Data = Default::default();

        assert_eq!(to_wasm(&conditional, &mut data), expected);
    }
}
