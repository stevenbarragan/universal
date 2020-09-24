extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "universal.pest"]
pub struct UniversalParser;

fn main() {}

type Variables = HashMap<String, String>;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Kind {
    Integer,
    Other(String)
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Language {
    Variable(String, Kind),
    Number(String),
    Infix(Operation, Box<Language>, Box<Language>)
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Operation {
    Add,Minus,Mult,Div,Eq
}

use pest::error::Error;
use pest::iterators::Pair;

fn build_ast(pair: Pair<Rule>, variables: &Variables) -> Result<Language, Error<Rule>> {
    println!("{:?}", pair);

    match pair.as_rule() {
        Rule::integer => {
            let value = pair.as_str().to_string();

            Ok(Language::Number(value))
        },
        Rule::variable => {
            let name = pair.as_str().to_string();
            let kind_str = variables.get(&name);

            if let Some(kind_name) = kind_str {
                let kind: Kind = match kind_name.as_str() {
                    "Int" => Kind::Integer,
                    x => Kind::Other(x.to_string())
                };

                Ok(Language::Variable(name, kind))
            } else {
                panic!("No variable found")
            }
        },
        Rule::variable_def => {
            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str().to_string();
            let a = inner.next().unwrap().as_str();

            let kind = match a {
                "Int" => Kind::Integer,
                x => Kind::Other(x.to_string())
            };

            Ok(Language::Variable(name, kind))
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
        }
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

        assert_eq!(to_ast("x: Int", &mut variables), Ok(Variable("x".to_string(), Kind::Integer)));

        variables.insert("x".to_string(), "Int".to_string());

        assert_eq!(to_ast("x", &mut variables), Ok(Variable("x".to_string(), Kind::Integer)));
    }

    #[test]
    fn operations() {
        let mut variables = HashMap::new();

        assert_eq!(to_ast("5 + 2", &mut variables),
            Ok(Infix( Operation::Add, Box::new(Number("5".to_string())), Box::new(Number("2".to_string())))));

        variables.insert("x".to_string(), "Int".to_string());

        assert_eq!(to_ast("x + 1", &mut variables),
            Ok(Infix(Operation::Add, Box::new(Variable ("x".to_string(), Kind::Integer)), Box::new(Number("1".to_string())))));
    }
}
