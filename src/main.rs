extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "universal.pest"]
pub struct UniversalParser;

fn main() {}

trait Wast {
    fn to_wast(&self) -> String;
}

struct VariableSetup {
    variable: String,
    kind: String,
}

impl Wast for VariableSetup {
    fn to_wast(&self) -> String {
        format!("(local ${} {})", self.variable, self.kind)
    }
}

struct VariableGet {
    variable: String,
}

impl Wast for VariableGet {
    fn to_wast(&self) -> String {
        format!("(local.get ${})", self.variable)
    }
}

use pest::error::Error;

pub fn parse_string(original: &str) -> Result<String, Error<Rule>> {
    let pair = UniversalParser::parse(Rule::instruction, original)?.next().unwrap();
    let mut instructions: Vec<Box<dyn Wast>> = Vec::new();

    for instruction in pair.into_inner() {
        match instruction.as_rule() {
            Rule::variable_setup => {
                let mut inner = instruction.into_inner();

                let variable = inner.next().unwrap().as_str().to_string();
                let kind = inner.next().unwrap().as_str().to_string();

                instructions.push(Box::new(VariableSetup { variable , kind }));
            },
            Rule::variable_get => {
                println!("{}", instruction);

                let mut inner = instruction.into_inner();
                let variable = inner.next().unwrap().as_str().to_string();

                instructions.push(Box::new(VariableGet { variable }));
            }
            _ => ()
        }
    }

    Ok(to_wasm(instructions))
}

fn to_wasm(instructions: Vec<Box<Wast>>) -> String {
    let mut result = String::new();

    for instruction in instructions {
        result += &(*instruction).to_wast();
    }

    result
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn variables() {
        assert_eq!(parse_string("let x: i32"), Ok("(local $x i32)".to_string()));

        assert_eq!(parse_string("x"), Ok("(local.get $x)".to_string()));
    }
}
