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
    Variable {
        name: String,
        kind: Kind
    },
    Number {
        value: String
    },
    Infix {
        left: Box<Language>,
        right: Box<Language>,
        operation: Operation
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Operation {
    Add,Minus,Multiply,
}

use pest::error::Error;
use pest::iterators::Pair;

fn build_ast(pair: Pair<Rule>) -> Result<Language, Error<Rule>> {
    println!("{:?}", pair);

    match pair.as_rule() {
        Rule::integer => {
            let value = pair.as_str().to_string();

            Ok(Language::Number{ value })
        },
        Rule::variable_def => {
            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str().to_string();
            let a = inner.next().unwrap().as_str();

            let kind = match a {
                "i32" => Kind::Integer,
                x => Kind::Other(x.to_string())
            };

            Ok(Language::Variable{ name, kind })
        },
        Rule::infix => {
            let mut inner = pair.into_inner();
            let left = build_ast(inner.next().unwrap())?;
            let message = inner.next().unwrap().as_str().to_string();
            let right = build_ast(inner.next().unwrap())?;

            Ok(Language::Infix{ left: Box::new(left), right: Box::new(right), operation: Operation::Add })
        }
        _ => panic!("WTF")
    }
}

pub fn to_ast(original: &str) -> Result<Language, Error<Rule>> {
    let pair = UniversalParser::parse(Rule::language, original)?.next().unwrap();

    build_ast(pair)
}

#[cfg(test)]
mod test {
    use super::*;
    use Language::*;

    #[test]
    fn constants() {
        assert_eq!(to_ast("42"), Ok(Number{ value: "42".to_string() }));
    }

    #[test]
    fn variables() {
        assert_eq!(to_ast("x: i32"), Ok(Variable{ name: "x".to_string(), kind: Kind::Integer }));
    }

    #[test]
    fn operations() {
        assert_eq!(to_ast("5 + 2"),
            Ok(Infix {
                left: Box::new(Number { value: "5".to_string() }),
                right: Box::new(Number { value: "2".to_string() }),
                operation: Operation::Add,
            }));
    }
}
