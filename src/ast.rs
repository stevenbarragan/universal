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
    Bool,
    Native(String)
}

pub type Variables = HashMap<String, ValueType>;
pub type Params = Vec<(String, ValueType)>;
pub type Block = Vec<Language>;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Operation {
    Add,
    And,
    Assignment,
    Div,
    Equal,
    Exp,
    LessThan,
    LessThanOrEq,
    Max,
    Min,
    Minus,
    Module,
    MoreThan,
    MoreThanOrEq,
    Mult,
    Native(String),
    NotEq,
    Or,
    XOr,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum ConditionalType {If, Unless}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Export {
    Function(String)
}

pub type Import = String;

#[derive(Debug, PartialEq, Clone, Hash, Eq, Copy)]
pub enum Visiblitity { Public, Private }

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Language {
    Block(Block),
    Boolean(bool),
    Call(String, Block, ValueType),
    Conditional(ConditionalType, Box<Language>, Box<Language>, Option<Box<Language>>),
    Float(String),
    Function(String, Params, Vec<ValueType>, Block, Visiblitity),
    Import(String),
    Infix(Operation, Box<Language>, Box<Language>),
    Module(String, Vec<Language>, Block, Vec<Export>, Vec<Import>),
    Number(i64),
    Symbol(String),
    Variable(String, ValueType),
}

pub fn find_value_type(node: &Language) -> &ValueType {
    match node {
        Language::Variable(_, value_type) => value_type,
        Language::Number(_) => &ValueType::Integer,
        Language::Float(_) => &ValueType::Float,
        Language::Infix(_, _, right) => {
            find_value_type(right)
        },
        Language::Function(_, _, results, _, _) => {
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
        Language::Module(_, _, instructions, _) => {
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
        },
        Language::Boolean(_) => &ValueType::Bool,
        Language::Import(_) => panic!("No value on import")
    }
}

use pest::error::Error;
use pest::iterators::{Pair,Pairs};

fn build_ast(pair: Pair<Rule>, variables: &mut Variables) -> Result<Language, Error<Rule>> {
    match pair.as_rule() {
        Rule::bool => {
            let value = pair.as_str();

            if value == "true" {
                Ok(Language::Boolean(true))
            } else {
                Ok(Language::Boolean(false))
            }
        },
        Rule::import => {
            let mut inner = pair.into_inner();

            let path = inner.next().unwrap().as_str();

            Ok(Language::Import(path.to_string()))
        },
        Rule::float => {
            let value = pair.as_str();

            Ok(Language::Float(value.to_owned()))
        },
        Rule::integer => {
            let value = pair.as_str().parse().unwrap();

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
        Rule::unary => {
            parse_expression(&mut pair.into_inner(), variables)
        },
        Rule::boolean => {
            parse_expression(&mut pair.into_inner(), variables)
        },
        Rule::expression => {
            parse_expression(&mut pair.into_inner(), variables)
        },
        Rule::summand => {
            parse_expression(&mut pair.into_inner(), variables)
        },
        Rule::primary => {
            let mut inner = pair.into_inner();

            build_ast(inner.next().unwrap(), variables)
        },
        Rule::function_def => {
            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str().to_string();

            let mut results = vec![];
            let mut params = vec![];
            let mut instructions = vec![];
            let mut visibility = Visiblitity::Private;

            let mut elem = inner.next().unwrap();

            if elem.as_rule() == Rule::public {
                visibility = Visiblitity::Public;

                elem = inner.next().unwrap();
            }

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

            Ok(Language::Function(name, params, results, instructions, visibility.clone()))
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

            if let Some(pair_params) = inner.next() {
                let mut params_inner = pair_params.into_inner();

                while let Some(param) = params_inner.next() {
                    params.push(build_ast(param, variables)?);
                }
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
            let mut exports = vec![];
            let mut imports = vec![];

            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str().to_string();

            let block = inner.next().unwrap();

            let mut pair = block.into_inner();

            while let Some(instruction) = pair.next() {
                let ast = build_ast(instruction, variables)?;

                match &ast {
                    Language::Function(name, _, _, _, visibility) => {
                        match visibility {
                            Visiblitity::Public => exports.push(Export::Function(name.clone())),
                            Visiblitity::Private => ()
                        }

                        functions.push(ast);
                    },
                    _ => instructions.push(ast)
                }
            }

            Ok(Language::Module(name, functions, instructions, exports))
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
        Rule::assignation => {
            let mut inner = pair.into_inner();

            let left = build_ast(inner.next().unwrap(), variables)?;
            let right = build_ast(inner.next().unwrap(), variables)?;

            Ok(Language::Infix(Operation::Assignment, Box::new(left), Box::new(right)))
        },
        x => panic!("WTF: {:?}", x)
    }
}

fn str_to_value_type(value_type: &str) -> ValueType {
    match value_type {
        "Int" => ValueType::Integer,
        "Float" => ValueType::Float,
        "Symbol" => ValueType::Symbol,
        kind => ValueType::Native(kind.to_string())
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

fn parse_expression(inner: &mut Pairs<Rule>, variables: &mut Variables) -> Result<Language, Error<Rule>> {
    let summand = build_ast(inner.next().unwrap(), variables)?;

    match inner.next() {
        Some(operator) => {
            match inner.next() {
                Some(pair) => {
                    let summand2 = build_ast(pair, variables)?;

                    let instruction = Language::Infix(
                        str_operator_to_enum(operator.as_str()),
                        Box::new(summand),
                        Box::new(summand2)
                    );

                    match inner.next() {
                        Some(second_operator) => {
                            Ok(Language::Infix(
                                    str_operator_to_enum(second_operator.as_str()),
                                    Box::new(instruction),
                                    Box::new(parse_expression(inner, variables)?)
                            ))
                        },
                        None => Ok(instruction)
                    }
                }
                None => panic!("seccond summand missing")
            }
        }
        None => Ok(summand)
    }
}

fn str_operator_to_enum(operator: &str) -> Operation {
    match operator {
        "+" => Operation::Add,
        "-" => Operation::Minus,
        "*" => Operation::Mult,
        "/" => Operation::Div,
        "^" => Operation::Exp,
        "=" => Operation::Assignment,
        "==" => Operation::Equal,
        "&&" => Operation::And,
        "||" => Operation::Or,
        "!=" => Operation::NotEq,
        "<" => Operation::LessThan,
        ">" => Operation::MoreThan,
        "<=" => Operation::LessThanOrEq,
        ">=" => Operation::MoreThanOrEq,
        "min" => Operation::Min,
        "max" => Operation::Max,
        op => Operation::Native(op.to_string())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use Language::*;

    #[test]
    fn constants() {
        let mut variables = HashMap::new();

        assert_eq!(to_ast("42", &mut variables), Ok(Number(42)));

        assert_eq!(to_ast("
                42
                ", &mut variables), Ok(Number(42)));
    }

    #[test]
    fn variables() {
        let mut variables = HashMap::new();

        variables.insert("x".to_string(), ValueType::Integer);

        assert_eq!(to_ast("x", &mut variables), Ok(Variable("x".to_string(), ValueType::Integer)));
    }

    #[test]
    fn infix() {
        let mut variables = HashMap::new();

        let instruction = "1 + 2 - 3";
        let expected = Infix(
            Operation::Minus,
            Box::new(Infix(Operation::Add,
                Box::new(Number(1)),
                Box::new(Number(2)),
            )),
            Box::new(Number(3))
        );

        assert_eq!(to_ast(instruction, &mut variables), Ok(expected))
    }

    #[test]
    fn function_call() {
        let mut variables = HashMap::new();

        variables.insert("add".to_string(), ValueType::Integer);

        assert_eq!(to_ast("add()", &mut variables), Ok(Call("add".to_string(), vec![], ValueType::Integer)));
        assert_eq!(to_ast("add(1)", &mut variables), Ok(Call("add".to_string(), vec![Number(1)], ValueType::Integer)));
    }

    #[test]
    fn operations() {
        let mut variables = HashMap::new();

        assert_eq!(to_ast("5 + 2", &mut variables),
            Ok(Infix( Operation::Add, Box::new(Number(5)), Box::new(Number(2)))));

        variables.insert("x".to_string(), ValueType::Integer);

        assert_eq!(to_ast("x + 1", &mut variables),
            Ok(Infix(Operation::Add, Box::new(Variable ("x".to_string(), ValueType::Integer)), Box::new(Number(1)))));
    }

    #[test]
    fn assignations() {
        let mut variables = HashMap::new();

        assert_eq!(to_ast("x: Int = 1", &mut variables),
            Ok(Infix(Operation::Assignment, Box::new(Variable ("x".to_string(), ValueType::Integer)), Box::new(Number(1)))));

        variables.insert("x".to_string(), ValueType::Integer);

        assert_eq!(to_ast("x = 1", &mut variables),
            Ok(Infix(Operation::Assignment, Box::new(Variable ("x".to_string(), ValueType::Integer)), Box::new(Number(1)))));
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
            vec![Number(42)],
            Visiblitity::Private
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
            vec![Number(42)],
            Visiblitity::Private
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
            vec![Variable("num".to_string(), ValueType::Integer)],
            Visiblitity::Private
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
            vec![Infix(Operation::Add, Box::new(Variable ("num".to_string(), ValueType::Integer)), Box::new(Number(2)))],
            Visiblitity::Private
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
            vec![Infix(Operation::Add, Box::new(Variable ("num".to_string(), ValueType::Integer)), Box::new(Variable ("num2".to_string(), ValueType::Integer)))],
            Visiblitity::Private
        );

        assert_eq!(result, Ok(expected));
    }

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

        let instructions = vec![Number(1)];

        let expected = Conditional(
            ConditionalType::If,
            Box::new(Number(1)),
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

        let instructions = vec![Number(2)];

        let expected = Conditional(
            ConditionalType::Unless,
            Box::new(Number(1)),
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

        let instructions = vec![Number(1)];
        let instructions_2 = vec![Number(2)];

        let expected = Conditional(
            ConditionalType::If,
            Box::new(Number(1)),
            Box::new(Language::Block(instructions)),
            Some(Box::new(Language::Block(instructions_2)))
        );

        assert_eq!(to_ast(program, &mut variables), Ok(expected));
    }

    #[test]
    fn conditionals_if_eq() {
        let mut variables = HashMap::new();

        let program = "
            if 1 == 1
              1
            end
            ";

        let expected = Conditional(
            ConditionalType::If,
            Box::new(Infix(Operation::Equal, Box::new(Number(1)), Box::new(Number(1)))),
            Box::new(Language::Block(vec![Number(1)])),
            None
        );

        assert_eq!(to_ast(program, &mut variables), Ok(expected));
    }

    #[test]
    fn booleans() {
        let mut variables = HashMap::new();

        let program = "true";
        let expected = Boolean(true);

        assert_eq!(to_ast(program, &mut variables), Ok(expected))
    }

    #[test]
    fn floats() {
        let mut variables = HashMap::new();

        let program = "2.23";
        let expected = Float("2.23".to_owned());

        assert_eq!(to_ast(program, &mut variables), Ok(expected))
    }
}
