extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "universal.pest"]
pub struct UniversalParser;

fn main() {}

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
    Kind(ValueType)
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
        _ => panic!("Value type undefined")
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

            let mut returns = vec![];
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

            if elem.as_rule() == Rule::returns {
                let mut pair = elem.into_inner();

                while let Some(rule) = pair.next() {
                    let kind = str_to_value_type(rule.as_str());

                    returns.push(kind);
                }

                elem = inner.next().unwrap();
            }

            if elem.as_rule() == Rule::block {
                let mut pair = elem.into_inner();

                while let Some(instruction) = pair.next() {
                    instructions.push(build_ast(instruction, variables)?);
                }
            }

            Ok(Language::Function(name, params, returns, instructions))
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
}
