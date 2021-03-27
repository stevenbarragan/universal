use crate::ast::*;
use std::collections::HashMap;

#[derive(Default)]
pub struct Data {
    pointers: HashMap<String, (usize, usize)>,
    memory: String,
    variables: Variables,
    modules: HashMap<String, Language>,
}

pub fn to_wasm(node: &Language, data: &mut Data) -> String {
    match node {
        Language::Boolean(thruty) => {
            let value = if *thruty { 1 } else { 0 };

            format!("(i32.const {})", value)
        }
        Language::Variable(name, _) => {
            format!("(local.get ${})", name)
        }
        Language::Number(number) => {
            format!("(i32.const {})", number)
        }
        Language::Import(_) => "".to_string(),
        Language::Float(float) => {
            format!("(f32.const {})", float)
        }
        Language::Infix(operation, left, right) => {
            if &Operation::Assignment == operation {
                if let Language::Variable(name, value_type) = left.as_ref() {
                    data.variables.insert(name.to_string(), value_type.clone());

                    return format!("{} (local.set ${})", to_wasm(right, data), name);
                }
            }

            let method = match operation {
                Operation::Add => "add",
                Operation::Minus => "sub",
                Operation::Mult => "mul",
                Operation::Div => "div",
                Operation::Exp => "div",
                Operation::Assignment => "set",
                Operation::Equal => "eq",
                Operation::And => "and",
                Operation::Or => "or",
                Operation::XOr => "xor",
                Operation::NotEq => "ne",
                Operation::LessThan => "lt",
                Operation::MoreThan => "gt",
                Operation::LessThanOrEq => "le_s",
                Operation::MoreThanOrEq => "ge",
                Operation::Min => "min",
                Operation::Max => "max",
                Operation::Module => "rem_s",
                Operation::Native(op) => op,
            };

            format!(
                "({}.{} {} {})",
                value_types_to_wasm(&find_value_type(left, &data.variables)),
                method,
                to_wasm(left, data),
                to_wasm(right, data)
            )
        }
        Language::Function(name, params, results, block, _) => {
            let params_str = params
                .into_iter()
                .map(|(name, value_type)| {
                    format!("(param ${} {})", name, value_type_to_wasm(value_type))
                })
                .collect::<Vec<String>>()
                .join(" ");

            let results = results
                .into_iter()
                .map(|value_type| format!("(result {})", value_type_to_wasm(value_type)))
                .collect::<Vec<String>>()
                .join(" ");

            let instructions = block
                .into_iter()
                .map(|language| to_wasm(language, data))
                .collect::<Vec<String>>()
                .join(" ");

            let locals = data
                .variables
                .iter()
                .map(|(name, value_type)| {
                    format!("(local ${} {})", name, value_types_to_wasm(&value_type))
                })
                .collect::<Vec<String>>()
                .join(" ");

            let body = vec![
                params_str.to_string(),
                results.to_string(),
                locals.to_string(),
                instructions.to_string(),
            ]
            .into_iter()
            .filter(|x| x != "")
            .collect::<Vec<String>>()
            .join(" ");

            let name_key = build_function_key(&name, &params);

            format!("(func ${} {})", name_key, body)
        }
        Language::Symbol(string) => {
            if let Some(position) = data.pointers.get(string) {
                format!("(i32.const {}) (i32.const {})", position.0, position.1)
            } else {
                let position = (data.memory.len(), string.len());

                data.pointers.insert(string.to_owned(), position);

                data.memory += string;

                format!("(i32.const {}) (i32.const {})", position.0, position.1)
            }
        }
        Language::Call(function_name, params, _) => {
            let params_str = params
                .into_iter()
                .map(|language| to_wasm(language, data))
                .collect::<Vec<String>>()
                .join(" ");

            let param_types = params
                .into_iter()
                .map(|param| find_value_type(param, &data.variables))
                .flatten()
                .collect::<Vec<ValueType>>();

            let name_key = function_key(&function_name, &param_types);

            format!("(call ${} {})", name_key, params_str)
        }
        Language::Block(instructions) => instructions
            .into_iter()
            .map(|language| to_wasm(language, data))
            .collect::<Vec<String>>()
            .join(" "),
        Language::Module(name, functions, instructions, exports, imports) => {
            let mut exports = exports.clone();

            data.modules.insert(name.to_owned(), node.clone());

            let imports_str = imports
                .into_iter()
                .map(|import_name| {
                    let module = data.modules.get(import_name).unwrap();

                    match module {
                        Language::Module(name, functions, instructions, exports, _imports) => {
                            exports
                                .into_iter()
                                .map(|export| match export {
                                    Export::Function(function_name, params, results) => {
                                        let results_str = value_types_to_wasm(results);

                                        let params_types = params
                                            .iter()
                                            .map(|param| param.1.clone())
                                            .collect::<Vec<ValueType>>();

                                        let params_str = if params.len() > 0 {
                                            let params_str = value_types_to_wasm(&params_types);

                                            format!("(param {})", params_str)
                                        } else {
                                            "".to_string()
                                        };

                                        let name_key = build_function_key(&function_name, &params);

                                        format!(
                                            "(import \"{}\" \"{}\" (func ${} {} (result {})))",
                                            import_name,
                                            name_key,
                                            name_key,
                                            params_str,
                                            results_str
                                        )
                                    }
                                    _ => unreachable!(),
                                })
                                .collect::<Vec<String>>()
                                .join(" ")
                        }
                        _ => unreachable!(),
                    }
                })
                .collect::<Vec<String>>()
                .join(" ");

            let functions_str = functions
                .into_iter()
                .map(|language| to_wasm(language, data))
                .collect::<Vec<String>>()
                .join(" ");

            let main = if instructions.is_empty() {
                "".to_string()
            } else {
                let value_types = match instructions.last() {
                    Some(instruction) => find_value_type(instruction, &data.variables),
                    None => panic!("No instructions"),
                };

                let main = Language::Function(
                    "main".to_string(),
                    vec![],
                    value_types.clone(),
                    instructions.clone(),
                    Visiblitity::Public,
                );

                exports.push(Export::Function(
                    "main".to_string(),
                    vec![],
                    value_types.clone(),
                ));

                to_wasm(&main, data)
            };

            let data_str = if data.memory.len() > 0 {
                format!(
                    "(import \"env\" \"memory\" (memory $env.memory 1)) (data (i32.const 0) \"{}\")",
                    data.memory
                )
            } else {
                "(import \"env\" \"memory\" (memory $env.memory 1))".to_string()
            };

            let exports_str = exports
                .iter()
                .map(|export| match export {
                    Export::Function(function_name, params, _) => {
                        let name_key = build_function_key(&function_name, &params);

                        format!("(export \"{}\" (func ${}))", name_key, name_key)
                    }
                })
                .collect::<Vec<String>>()
                .join(" ");

            let body = vec![imports_str, data_str, functions_str, main, exports_str]
                .into_iter()
                .filter(|x| x != "")
                .collect::<Vec<String>>()
                .join(" ");

            format!("(module ${} {})", name, body)
        }
        Language::Conditional(conditional_type, instruction, block, block2) => {
            let conditional_str = to_wasm(instruction, data);
            let block_str = to_wasm(block, data);

            match conditional_type {
                ConditionalType::If => match block2 {
                    Some(x) => format!(
                        "(if (result {}) {} (then {}) (else {}))",
                        value_types_to_wasm(&find_value_type(block, &data.variables)),
                        conditional_str,
                        block_str,
                        to_wasm(x, data)
                    ),
                    None => format!("(if {} (then {}))", conditional_str, block_str),
                },
                ConditionalType::Unless => match block2 {
                    Some(x) => format!(
                        "(if (result {}) (i32.eqz {}) (then {}) (else {}))",
                        value_types_to_wasm(&find_value_type(block, &data.variables)),
                        conditional_str,
                        block_str,
                        to_wasm(x, data)
                    ),
                    None => format!("(if (i32.eqz {}) (then {}))", conditional_str, block_str),
                },
            }
        }
        Language::Array(instructions) => {
            let name = new_variable_name(&data);

            let instruction_size: usize = match instructions.first() {
                Some(instruction) => {
                    let value_type = find_value_type(&instruction, &data.variables);

                    data.variables.insert(name.to_string(), vec![ValueType::Array(value_type.clone())]);

                    size(&value_type)
                },
                None => {
                    data.variables.insert(name.to_string(), vec![ValueType::Array(vec![ValueType::Integer])]);

                    0
                }
            };

            let mut result = format!("(local.tee ${} (call $calloc_int_int (i32.const {}) (i32.const {})))", name, instruction_size, instructions.len());

            if instructions.len() > 0 {
                result = format!("{} {}", result, instructions.into_iter().enumerate()
                    .map(|(index, instruction)| format!("(local.get ${}) {} (i32.store offset={})", name, to_wasm(instruction, data), index * instruction_size)) 
                    .collect::<Vec<String>>()
                    .join(" "));
            }

            result.to_string()
        }
        Language::ArrayAccess(name, index) => {
            if let Some(value_type) = data.variables.get(name) {
                let offset = index * size(&value_type);

                format!("(i32.load offset={} (local.get ${}))", offset, name)
            } else {
                panic!("Variable {} not found", name)
            }
        }
        Language::Program(_) => "".to_string(),
    }
}

fn new_variable_name(data: &Data) -> String {
    let mut index = 0;
    let mut new_name = format!("l{}", &index);

    while data.variables.contains_key(&new_name) {
        index += 1;

        new_name = format!("l{}", index);
    }

    new_name
}

fn size(value_types: &Vec<ValueType>) -> usize {
    value_types.into_iter()
        .map(|value_type| match value_type {
            ValueType::Bool => 1,
            ValueType::Float => 1,
            ValueType::Integer => 4,
            ValueType::Native(name) => 1, // fix me!
            ValueType::Symbol => 8,
            ValueType::Array(_) => 4
        })
        .sum()
}

fn value_types_to_wasm(value_types: &Vec<ValueType>) -> String {
    value_types
        .iter()
        .map(|value_type: &ValueType| value_type_to_wasm(value_type))
        .collect::<Vec<String>>()
        .join(" ")
}

fn value_type_to_wasm(value_type: &ValueType) -> String {
    match value_type {
        ValueType::Integer => "i32".to_string(),
        ValueType::Float => "f32".to_string(),
        ValueType::Symbol => "i32 i32".to_string(),
        ValueType::Bool => "i32".to_string(),
        ValueType::Native(name) => name.to_string(),
        ValueType::Array(_value_type) => "i32".to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use Language::*;

    #[test]
    fn function_to_wasm() {
        let mut data: Data = Default::default();

        let function = Function(
            "add2".to_string(),
            vec![("num".to_string(), ValueType::Integer)],
            vec![ValueType::Integer],
            vec![Infix(
                Operation::Add,
                Box::new(Variable("num".to_string(), vec![ValueType::Integer])),
                Box::new(Number(2)),
            )],
            Visiblitity::Private,
        );

        assert_eq!(to_wasm(&function, &mut data), "(func $add2_int (param $num i32) (result i32) (i32.add (local.get $num) (i32.const 2)))");

        let function_ast = to_ast(
            "fn tres: Int
               num: Int = 3
             end",
        );

        match function_ast {
            Ok(function) => {
                assert_eq!(
                    to_wasm(&function, &mut data),
                    "(func $tres (result i32) (local $num i32) (i32.const 3) (local.set $num))"
                );
            }
            Err(e) => println!("{}", e),
        }

        let function_ast = to_ast(
            "fn tres(x: Int): Int
              num: Int = 3
              x + num
            end",
        );

        match function_ast {
            Ok(function) => {
                assert_eq!(to_wasm(&function, &mut data),
                    "(func $tres_int (param $x i32) (result i32) (local $num i32) (i32.const 3) (local.set $num) (i32.add (local.get $x) (local.get $num)))");
            }
            Err(e) => println!("{}", e),
        }
    }

    #[test]
    fn assignation_to_wasm() {
        let mut data: Data = Default::default();
        let assignation = Infix(
            Operation::Assignment,
            Box::new(Variable("x".to_string(), vec![ValueType::Integer])),
            Box::new(Number(1)),
        );

        assert_eq!(
            to_wasm(&assignation, &mut data),
            "(i32.const 1) (local.set $x)"
        )
    }

    #[test]
    fn modules() {
        let mut data: Data = Default::default();

        let function = Function(
            "tres".to_string(),
            vec![],
            vec![ValueType::Integer],
            vec![Language::Number(3)],
            Visiblitity::Public,
        );

        let mut exports = vec![];
        let imports = vec![];

        exports.push(Export::Function(
            "tres".to_string(),
            vec![],
            vec![ValueType::Integer],
        ));

        let module = Language::Module(
            "awesome".to_string(),
            vec![function],
            vec![Number(42)],
            exports,
            imports,
        );

        let expected = "(module $awesome (import \"env\" \"memory\" (memory $env.memory 1)) (func $tres (result i32) (i32.const 3)) (func $main (result i32) (i32.const 42)) (export \"tres\" (func $tres)) (export \"main\" (func $main)))";

        assert_eq!(to_wasm(&module, &mut data), expected);
    }

    #[test]
    fn symbols() {
        let mut data: Data = Default::default();
        let exports = vec![];
        let imports = vec![];

        let module = Language::Module(
            "awesome".to_string(),
            vec![],
            vec![Symbol("42".to_string())],
            exports,
            imports,
        );

        let expected = "(module $awesome (import \"env\" \"memory\" (memory $env.memory 1)) (data (i32.const 0) \"42\") (func $main (result i32 i32) (i32.const 0) (i32.const 2)) (export \"main\" (func $main)))";

        assert_eq!(to_wasm(&module, &mut data), expected);

        let mut data: Data = Default::default();
        let exports = vec![];
        let imports = vec![];

        let module = Language::Module(
            "awesome".to_string(),
            vec![],
            vec![Symbol("42".to_string()), Symbol("43".to_string())],
            exports,
            imports,
        );

        let expected = "(module $awesome (import \"env\" \"memory\" (memory $env.memory 1)) (data (i32.const 0) \"4243\") (func $main (result i32 i32) (i32.const 0) (i32.const 2) (i32.const 2) (i32.const 2)) (export \"main\" (func $main)))";

        assert_eq!(to_wasm(&module, &mut data), expected)
    }

    #[test]
    fn conditional_if() {
        let instructions = vec![Number(1)];

        let conditional = Conditional(
            ConditionalType::If,
            Box::new(Number(1)),
            Box::new(Language::Block(instructions)),
            None,
        );

        let expected = "(if (i32.const 1) (then (i32.const 1)))";

        let mut data: Data = Default::default();

        assert_eq!(to_wasm(&conditional, &mut data), expected);
    }

    #[test]
    fn conditional_if_else() {
        let conditional = Conditional(
            ConditionalType::If,
            Box::new(Number(1)),
            Box::new(Language::Block(vec![Number(1)])),
            Some(Box::new(Language::Block(vec![Number(2)]))),
        );

        let expected = "(if (result i32) (i32.const 1) (then (i32.const 1)) (else (i32.const 2)))";

        let mut data: Data = Default::default();

        assert_eq!(to_wasm(&conditional, &mut data), expected);
    }

    #[test]
    fn conditional_unless() {
        let instructions = vec![Number(1)];

        let conditional = Conditional(
            ConditionalType::Unless,
            Box::new(Number(1)),
            Box::new(Language::Block(instructions)),
            None,
        );

        let expected = "(if (i32.eqz (i32.const 1)) (then (i32.const 1)))";

        let mut data: Data = Default::default();

        assert_eq!(to_wasm(&conditional, &mut data), expected);
    }

    #[test]
    fn booleans() {
        let mut data: Data = Default::default();

        let instruction = Boolean(true);

        let expected = "(i32.const 1)";

        assert_eq!(to_wasm(&instruction, &mut data), expected);

        let instruction = Boolean(false);

        let expected = "(i32.const 0)";

        assert_eq!(to_wasm(&instruction, &mut data), expected);
    }

    #[test]
    fn float() {
        let mut data: Data = Default::default();

        let instruction = Float("2.23".to_string());

        let expected = "(f32.const 2.23)";

        assert_eq!(to_wasm(&instruction, &mut data), expected);
    }

    #[test]
    fn arrays() {
        let mut data: Data = Default::default();

        let instruction = Array(vec![]);
        let expected = "(local.tee $l0 (call $calloc_int_int (i32.const 0) (i32.const 0)))";
        
        assert_eq!(to_wasm(&instruction, &mut data), expected);

        let mut data: Data = Default::default();
        let array_type = ValueType::Array(vec![ValueType::Integer]);
        let instruction = Array(vec![Number(42)]);

        let expected = "(local.tee $l0 (call $calloc_int_int (i32.const 4) (i32.const 1))) (local.get $l0) (i32.const 42) (i32.store offset=0)";
        
        assert_eq!(to_wasm(&instruction, &mut data), expected);

        let mut data: Data = Default::default();
        let array_type = ValueType::Array(vec![ValueType::Integer]);
        let instruction = Infix(
            Operation::Assignment,
            Box::new(Variable("a".to_string(), vec![array_type])),
            Box::new(Array(vec![Number(42)]))
        );

        let expected = "(local.tee $l0 (call $calloc_int_int (i32.const 4) (i32.const 1))) (local.get $l0) (i32.const 42) (i32.store offset=0) (local.set $a)";
        
        assert_eq!(to_wasm(&instruction, &mut data), expected);

        let instruction = ArrayAccess("a".to_string(), 0);
        let expected = "(i32.load offset=0 (local.get $a))";
        
        assert_eq!(to_wasm(&instruction, &mut data), expected);
    }
}
