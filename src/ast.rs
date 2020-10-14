use std::collections::HashMap;
use pest::Parser;

#[derive(Parser)]
#[grammar = "universal.pest"]
pub struct UniversalParser;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum ValueType {
    Integer,
    Float,
    Symbol,
}

pub type Variables = HashMap<String, ValueType>;
pub type Params = Vec<(String, ValueType)>;
pub type Block = Vec<Language>;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Operation {
    Add,
    Minus,
    Mult,
    Div,
    Eq
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum ConditionalType {If, Unless}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Language {
    Block(Block),
    Call(String, Block, ValueType),
    Function(String, Params, Vec<ValueType>, Block),
    Infix(Operation, Box<Language>, Box<Language>),
    Module(String, Vec<Language>, Block),
    Number(String),
    Variable(String, ValueType),
    Symbol(String),
    Conditional(ConditionalType, Box<Language>, Box<Language>, Option<Box<Language>>)
}

pub fn find_value_type(node: &Language) -> &ValueType {
    match node {
        Language::Variable(_, value_type) => value_type,
        Language::Number(_) => &ValueType::Integer,
        Language::Infix(_, _, right) => {
            find_value_type(right)
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
        },
        Language::Symbol(_) => &ValueType::Symbol,
        Language::Conditional(_, _, _, instructions) => {
            if let Some(instruction) = instructions {
                find_value_type(instruction) 
            } else {
                panic!("No instructions")
            }
        }
    }
}

use pest::error::Error;
use pest::iterators::Pair;

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
        Rule::block => {
            let mut instructions = vec![];
            let mut inner = pair.into_inner();

            while let Some(instruction) = inner.next() {
                instructions.push(build_ast(instruction, variables)?);
            }

            Ok(Language::Block(instructions))
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
        },
        Rule::symbol => {
            let mut inner = pair.into_inner();

            let string = inner.next().unwrap();

            Ok(Language::Symbol(string.as_str().to_string()))
        },
        Rule::conditional => {
            let mut inner = pair.into_inner();

            let kind = match inner.next().unwrap().as_str() {
                "if" => ConditionalType::If, 
                "unless" => ConditionalType::Unless, 
                x => panic!("{} not supported", x)
            };
            
            let instruction = build_ast(inner.next().unwrap(), variables)?;
            let block = Box::new(build_ast(inner.next().unwrap(), variables)?);

            let block2 = if let Some(else_block) = inner.next() {
                Some(Box::new(build_ast(else_block, variables)?))
            } else {
                None
            };

            Ok(Language::Conditional(kind, Box::new(instruction), block, block2))
        },
        x => panic!("WTF: {:?}", x)
    }
}

fn str_to_value_type(value_type: &str) -> ValueType {
    match value_type {
        "Int" => ValueType::Integer,
        "Float" => ValueType::Float,
        "Symbol" => ValueType::Symbol,
        kind => panic!("Value type {:?} not supported", kind)
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


#[cfg(test)]
mod test {
    use super::*;
    use Language::*;

    #[test]
    fn strings() {
        let mut variables = HashMap::new();

        assert_eq!(to_ast(":42", &mut variables), Ok(Symbol("42".to_string())));
        assert_eq!(to_ast(":\"steven barragan\"", &mut variables), Ok(Symbol("steven barragan".to_string())));
    }

    #[test]
    fn conditionals_if() {
        let mut variables = HashMap::new();

        let program = "
            if 1
              1
            end
            ";

        let instructions = vec![Number("1".to_string())];

        let expected = Conditional(
            ConditionalType::If,
            Box::new(Number("1".to_string())),
            Box::new(Language::Block(instructions)),
            None
        );

        assert_eq!(to_ast(program, &mut variables), Ok(expected));
    }

    #[test]
    fn conditionals_unless() {
        let mut variables = HashMap::new();

        let program = "
            unless 1
              2
            end
            ";

        let instructions = vec![Number("2".to_string())];

        let expected = Conditional(
            ConditionalType::Unless,
            Box::new(Number("1".to_string())),
            Box::new(Language::Block(instructions)),
            None
        );

        assert_eq!(to_ast(program, &mut variables), Ok(expected));
    }

    #[test]
    fn conditionals_if_else() {
        let mut variables = HashMap::new();

        let program = "
            if 1
              1
            else
              2
            end
        ";

        let instructions = vec![Number("1".to_string())];
        let instructions_2 = vec![Number("2".to_string())];

        let expected = Conditional(
            ConditionalType::If,
            Box::new(Number("1".to_string())),
            Box::new(Language::Block(instructions)),
            Some(Box::new(Language::Block(instructions_2)))
        );

        assert_eq!(to_ast(program, &mut variables), Ok(expected));
    }
}
