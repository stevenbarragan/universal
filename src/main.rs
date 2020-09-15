extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "universal.pest"]
pub struct UniversalParser;

mod instruction;

fn main() {}

type Name = String;

type Variables = HashMap<String, String>;

struct Variable {
    name: String,
    kind: String,
}

#[derive(PartialEq)]
enum Language {
    BasicOperation,
    ConstantGet,
    NativeType,
    Number,
    NumberGet,
    VariableGet,
    VariableName,
    VariableSet,
    ParamSet,
    FunctionSet,
    Result,
}

struct Instruction {
    kind: Language,
    name: Name,
    message: Option<String>,
    params: Vec<Instruction>,
    valueType: String
}

impl Instruction {
    fn to_wast(&self) -> String {
        if self.kind == Language::VariableName {
            return format!("${}", self.name)
        }

        let mut parts: Vec<String> = vec![];

        if let Some(message) = &self.message {
            parts.push(format!("{}.{}", &self.name, message))
        } else {
            if self.kind == Language::FunctionSet {
                parts.push(format!("${}", &self.name))
            } else {
                parts.push(format!("{}", &self.name))
            }
        }

        for param in &self.params {
            parts.push(param.to_wast());
        }

        let mut result = parts.join(" ");

        if parts.len() > 1 {
            result = format!("({})", result);
        }

        result
    }
}

use pest::error::Error;
use pest::iterators::Pair;

fn build_instruction(pair: Pair<Rule>, variables: &Variables) -> Result<Instruction, Error<Rule>> {
    match pair.as_rule() {
        Rule::variable_setup => {
            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str();
            let kind = inner.next().unwrap().as_str();

            let instruction = Instruction {
                kind: Language::VariableSet,
                name: "local".to_string(),
                message: None,
                valueType: kind.to_owned(),
                params: vec![
                    Instruction {
                        kind: Language::VariableName,
                        name: name.to_string(),
                        message: None,
                        valueType: kind.to_owned(),
                        params: vec![],
                    },
                    Instruction {
                        kind: Language::NativeType,
                        name: kind.to_string(),
                        message: None,
                        valueType: kind.to_owned(),
                        params: vec![],
                    },
                ]
            };

            Ok(instruction)
        },
        Rule::variable_get => {
            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str();
            let variable = variables.get(name);

            if let Some(variable) = variable {
                let instruction = Instruction {
                    kind: Language::VariableGet,
                    name: "local".to_string(),
                    message: Some("get".to_string()),
                    valueType: variable.to_string(),
                    params: vec![
                        Instruction{
                            kind: Language::VariableName,
                            name: name.to_string(),
                            message: None,
                            valueType: variable.to_string(),
                            params: vec![]
                        },
                    ]
                };

                Ok(instruction)
            } else {
                panic!("No variable found")
            }
        },
        Rule::number_get => {
            let mut inner = pair.into_inner();
            let value = inner.next().unwrap().as_str();

            let instruction = Instruction {
                kind: Language::NumberGet,
                name: "i32".to_string(),
                message: Some("const".to_string()),
                valueType: "i32".to_string(),
                params: vec![
                    Instruction {
                        kind: Language::Number,
                        name: value.to_string(),
                        message: None,
                        valueType: "i32".to_string(),
                        params: vec![]
                    }
                ]
            };

            Ok(instruction)
        },
        Rule::function_set => {
            let mut params = vec![];

            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str();
            let params_inner = inner.next().unwrap().into_inner();

            for pair in params_inner {
                let mut inner_rules = pair.into_inner();

                let name = inner_rules.next().unwrap().as_str();
                let kind = inner_rules.next().unwrap().as_str();

                params.push(
                    Instruction {
                        kind: Language::ParamSet,
                        name: "param".to_owned(),
                        message: None,
                        valueType: kind.to_owned(),
                        params: vec![
                            Instruction {
                                kind: Language::VariableName,
                                name: name.to_string(),
                                message: None,
                                valueType: kind.to_owned(),
                                params: vec![],
                            }
                        ]
                    }
                )
            }

            let kind = inner.next().unwrap().as_str();

            params.push(Instruction {
                kind: Language::Result,
                name: "result".to_owned(),
                message: None,
                valueType: kind.to_owned(),
                params: vec![
                    Instruction {
                        kind: Language::NativeType,
                        name: kind.to_string(),
                        message: None,
                        valueType: kind.to_owned(),
                        params: vec![],
                    }
                ]
            });

            let instruction = Instruction {
                kind: Language::FunctionSet,
                name: name.to_string(),
                message: None,
                valueType: kind.to_string(),
                params,
            };

            Ok(instruction)
        },
        Rule::basic_operation => {
            let mut inner = pair.into_inner();

            let first_pair = inner.next().unwrap();
            let mut params: Vec<Instruction> = first_pair.into_inner()
                .map(|pair|
                    match build_instruction(pair, variables) {
                        Ok(instruction) => instruction,
                        _ => panic!("WTF!")
                    })
                .collect();


            let method = match inner.next().unwrap().as_str() {
                "+" => "sum",
                "-" => "minus",
                "*" => "mul",
                "/" => "div",
                method => method
            };


            let second_pair = inner.next().unwrap();
            let mut second_instructions: Vec<Instruction> = second_pair.into_inner()
                .map(|pair|
                    match build_instruction(pair, variables) {
                        Ok(instruction) => instruction,
                        _ => panic!("WTF!")
                    })
                .collect();

            params.append(&mut second_instructions);

            let instruction = Instruction {
                kind: Language::BasicOperation,
                name: params[0].valueType.to_string(),
                message: Some(method.to_string()),
                valueType: params[0].valueType.to_string(),
                params
            };

            Ok(instruction)
        }
        _ => panic!("WTF")
    }
}

fn build_block(pair: Pair<Rule>) -> Result<Vec<Instruction>, Error<Rule>> {
    let mut variables: Variables = HashMap::new();
    let mut instructions: Vec<Instruction> = vec![];

    for rule in pair.into_inner() {
        let instruction = build_instruction(rule, &variables)?;

        if instruction.kind == Language::VariableSet {
            let name = instruction.params[0].name.to_string();
            let value_type = instruction.params[1].name.to_string();

            variables.insert(name, value_type);
        }

        instructions.push(instruction);
    }

    Ok(instructions)
}

pub fn parse_string(original: &str) -> Result<String, Error<Rule>> {
    let pair = UniversalParser::parse(Rule::instructions, original)?.next().unwrap();

    let instructions = build_block(pair)?;

    Ok(to_wasm(instructions))
}


fn to_wasm(instructions: Vec<Instruction>) -> String {
    let mut result: Vec<String> = vec![];

    for instruction in instructions {
        result.push(instruction.to_wast());
    }

    result.join(" ")
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn variables() {
        assert_eq!(parse_string("let x: i32"), Ok("(local $x i32)".to_string()));

        assert_eq!(parse_string("let x: i32; x"), Ok("(local $x i32) (local.get $x)".to_string()));
    }

    #[test]
    fn constants() {
        assert_eq!(parse_string("42"), Ok("(i32.const 42)".to_string()));
    }

    #[test]
    fn function() {
        assert_eq!(parse_string("
                    fn add(x: i32, y: i32) : i32
                "), Ok("($add (param $x) (param $y) (result i32))".to_string()));
    }

    // #[test]
    // fn operations() {
    //     assert_eq!(parse_string("5 + 10"), Ok("(i32.sum (i32.const 5) (i32.const 10))".to_string()));
    // }

    #[test]
    fn operations() {
        assert_eq!(parse_string("5 + 10"), Ok("(i32.sum (i32.const 5) (i32.const 10))".to_string()));
    }
}
