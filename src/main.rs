extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;
use std::fs;

#[derive(Parser)]
#[grammar = "universal.pest"]
pub struct UniversalParser;

fn main() {}

use pest::error::Error;

pub fn parse_string(original: &str) -> Result<String, Error<Rule>> {
    let pair = UniversalParser::parse(Rule::instruction, original)?.next().unwrap();
    let mut result = String::from("");

    for instruction in pair.into_inner() {
        match instruction.as_rule() {
            Rule::variable_setup => {
                let mut inner = instruction.into_inner();

                let variable = inner.next().unwrap().as_str();
                let kind = inner.next().unwrap().as_str();

                result = format!("(local ${} {})", variable, kind);
            },
            _ => ()
        }
    }

    Ok(result)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn variables() {
        assert_eq!(parse_string("let x: i32"), Ok("(local $x i32)".to_string()))
    }
}
