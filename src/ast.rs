use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum ValueType {
    Integer,
    Float,
}

pub type Variables = HashMap<String, ValueType>;
pub type Params = Vec<(String, ValueType)>;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Operation {
    Add,
    Minus,
    Mult,
    Div,
    Eq
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Language {
    Block(Vec<Language>),
    Call(String, Vec<Language>, ValueType),
    Function(String, Params, Vec<ValueType>, Vec<Language>),
    Infix(Operation, Box<Language>, Box<Language>),
    Module(String, Vec<Language>, Vec<Language>),
    Number(String),
    Variable(String, ValueType),
}
