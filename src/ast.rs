use indexmap::IndexMap;
use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use std::fmt;
use std::fs;
use std::path::Path;

use crate::utils::*;

#[derive(Parser)]
#[grammar = "universal.pest"]
pub struct UniversalParser;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
#[allow(non_camel_case_types)]
pub enum Native {
    i32,
    i64,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum ValueType {
    Bool,
    Float,
    Integer,
    Native(Native),
    Symbol,
    Array(Vec<ValueType>),
    CustomType(Name, Vec<ValueType>),
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        match self {
            ValueType::Bool => f.write_str("bool"),
            ValueType::Float => f.write_str("float"),
            ValueType::Integer => f.write_str("int"),
            ValueType::Native(native_type) => match native_type {
                Native::i32 => f.write_str("i32"),
                Native::i64 => f.write_str("i64"),
            },
            ValueType::Symbol => f.write_str("symbol"),
            ValueType::Array(_value_type) => f.write_str("array"),
            ValueType::CustomType(name, _types) => f.write_str(name),
        }
    }
}

pub type Block = Vec<Language>;
pub type Modules = Vec<Language>;
pub type Params = Vec<(String, ValueType)>;
pub type Results = Vec<ValueType>;

pub type Variables = IndexMap<String, Vec<ValueType>>;
pub type Methods = IndexMap<String, Vec<ValueType>>;

pub struct Context {
    variables: Vec<Variables>,
    types_attributes: IndexMap<Name, Attributes>,
    types_methods: IndexMap<Name, Methods>,
    selfs: Vec<String>,
}

impl Default for Context {
    fn default() -> Self {
        Context {
            variables: vec![Variables::new()],
            types_attributes: IndexMap::new(),
            types_methods: IndexMap::new(),
            selfs: vec!["".to_string()],
        }
    }
}

impl Context {
    pub fn find_type_attribute(&self, name: &str) -> Attributes {
        self.types_attributes.get(name).unwrap().clone()
    }

    pub fn add_self(&mut self, name: &str) {
        self.selfs.push(name.to_string());
    }

    pub fn pop_self(&mut self) {
        self.selfs.pop();
    }

    pub fn add_type_attributes(&mut self, name: &str, attributes: &Attributes) {
        self.types_attributes
            .insert(name.to_string(), attributes.clone());
    }

    pub fn add_type_methods(&mut self, name: &str, method_name: &str, results: &Results) {
        if let Some(type_methods) = self.types_methods.get_mut(name) {
            type_methods.insert(method_name.to_string(), results.clone());
        } else {
            let mut type_methods = IndexMap::new();

            type_methods.insert(method_name.to_string(), results.clone());

            self.types_methods.insert(name.to_string(), type_methods);
        };
    }

    pub fn add_variable(&mut self, key: String, value: Vec<ValueType>) {
        if let Some(scope) = self.variables.last_mut() {
            scope.insert(key, value);
        } else {
            let mut new_scope = Variables::new();

            new_scope.insert(key, value);

            self.variables.push(new_scope);
        }
    }

    pub fn add_new_scope(&mut self) {
        self.variables.push(Variables::new());
    }

    pub fn destroy_scope(&mut self) {
        self.variables.pop();
    }

    pub fn find_variable(&self, key: &String) -> Option<Vec<ValueType>> {
        for variables in self.variables.iter().rev() {
            if let Some(value) = variables.get(key) {
                return Some(value.clone());
            }
        }

        None
    }

    pub fn variable_exists(&self, key: &String) -> bool {
        for variables in self.variables.iter() {
            if variables.contains_key(key) {
                return true;
            }
        }

        false
    }

    pub fn local_variables(&self) -> Variables {
        if let Some(variables) = self.variables.last() {
            variables.clone()
        } else {
            Variables::new()
        }
    }

    fn find_type_attribute_type(
        &self,
        variable: &String,
        attribute: &String,
        scope: &Context,
    ) -> Option<ValueType> {
        let variable_types = scope.find_variable(variable).unwrap();

        if let Some(ValueType::CustomType(name, _types)) = variable_types.first() {
            if let Some(attributes) = self.types_attributes.get(name) {
                if let Some(kind) = attributes.get(attribute) {
                    return Some(kind.clone());
                }
            }
        }

        None
    }

    fn find_custom_caller_type(
        &self,
        variable: &String,
        attribute: &String,
        scope: &Context,
    ) -> Option<Vec<ValueType>> {
        let variable_types = scope.find_variable(variable).unwrap();

        if let Some(ValueType::CustomType(name, _types)) = variable_types.first() {
            let methods = self.types_methods.get(name).unwrap();

            for (name, types) in methods {
                if name == attribute {
                    return Some(types.clone());
                }
            }
        }

        None
    }

    pub fn find_type_method_type(&self, kind: &String, message: &String) -> Vec<ValueType> {
        let method = self.types_methods.get(kind).expect("custom type not found");

        method
            .get(message)
            .expect("Custom type method not found")
            .clone()
    }

    pub fn calculate_memory_offset(&self, kind: &String, attribute: &String) -> usize {
        let attributes = self
            .types_attributes
            .get(kind)
            .expect("custom type not found");

        let mut value_types = vec![];

        for (name, value_type) in attributes {
            if name == attribute {
                break;
            } else {
                value_types.push(value_type.clone())
            }
        }

        size(&value_types)
    }

    pub fn get_self(&self) -> String {
        self.selfs.last().expect("No self found").to_string()
    }
}

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
pub enum ConditionalType {
    If,
    Unless,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Export {
    Function(String, Params, Results),
}

pub type Import = String;

#[derive(Debug, PartialEq, Clone, Hash, Eq, Copy)]
pub enum Visiblitity {
    Public,
    Private,
}

pub type Name = String;
pub type Callee = String;
pub type Message = String;
pub type Attributes = IndexMap<Name, ValueType>;
pub type NamedTypes = Vec<Name>;
pub type Parameters = Vec<Language>;
pub type Functions = Vec<Language>;
pub type Types = Vec<Language>;

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Language {
    Block(Block),
    Boolean(bool),
    Call(String, Parameters, Results),
    Conditional(
        ConditionalType,
        Box<Language>,
        Box<Language>,
        Option<Box<Language>>,
    ),
    Float(String),
    Function(String, Params, Results, Block, Visiblitity),
    Import(Import),
    Infix(Operation, Box<Language>, Box<Language>),
    Module(
        String,
        Vec<Language>,
        Block,
        Vec<Export>,
        Vec<Import>,
        Types,
    ),
    Number(i64),
    Program(Vec<Language>),
    Symbol(String),
    CustomType(Name, NamedTypes, Attributes, Functions),
    TypeAttributeAccess(Callee, Message),
    TypeCall(Callee, Message, Parameters),
    TypeInstance(Name, NamedTypes, IndexMap<Name, Language>),
    Variable(String, Vec<ValueType>),
    Array(Vec<Language>),
    ArrayAccess(String, usize),
    SelfMethodAccess(Message, Parameters),
    SelfAttributeAccess(Message),
}

pub fn find_value_type(node: &Language, scope: &Context) -> Vec<ValueType> {
    match node {
        Language::Variable(_, value_type) => value_type.clone(),
        Language::Number(_) => vec![ValueType::Native(Native::i32)],
        Language::Float(_) => vec![ValueType::Float],
        Language::Infix(_, _, right) => find_value_type(right, scope),
        Language::Function(_, _, results, _, _) => results.clone(),
        Language::Call(_, _, value_type) => value_type.clone(),
        Language::Block(instructions) => {
            if let Some(instruction) = instructions.last() {
                find_value_type(instruction, scope)
            } else {
                panic!("No instructions")
            }
        }
        Language::Module(_, _, instructions, _, _, _) => {
            if let Some(instruction) = instructions.last() {
                find_value_type(instruction, scope)
            } else {
                panic!("No instructions")
            }
        }
        Language::Symbol(_) => vec![ValueType::Symbol],
        Language::Conditional(_, _, _, instructions) => {
            if let Some(instruction) = instructions {
                find_value_type(instruction, scope)
            } else {
                panic!("No instructions")
            }
        }
        Language::Boolean(_) => vec![ValueType::Bool],
        Language::Import(_) => panic!("No value type for import"),
        Language::Program(_) => panic!("No value type for program"),
        Language::Array(instructions) => {
            let array_type = if let Some(instruction) = instructions.first() {
                find_value_type(instruction, scope)
            } else {
                // Default array_type
                vec![ValueType::Integer]
            };

            vec![ValueType::Array(array_type)]
        }
        Language::ArrayAccess(name, _index) => {
            if let Some(kinds) = scope.find_variable(name) {
                kinds
                    .into_iter()
                    .map(|kind| {
                        if let ValueType::Array(array_types) = kind {
                            array_types.clone()
                        } else {
                            panic!("Variable {} is not an array", name)
                        }
                    })
                    .flatten()
                    .collect::<Vec<ValueType>>()
            } else {
                panic!("variable: ${} not found", name)
            }
        }
        Language::CustomType(name, _named_types, attributes, _functions) => {
            let types = attributes
                .iter()
                .map(|(_name, kind)| kind.clone())
                .collect::<Vec<ValueType>>();

            vec![ValueType::CustomType(name.clone(), types)]
        }
        Language::TypeAttributeAccess(callee, message) => {
            vec![scope
                .find_type_attribute_type(callee, message, scope)
                .unwrap()]
        }
        Language::TypeCall(callee, message, _parameters) => scope
            .find_custom_caller_type(callee, message, scope)
            .unwrap(),
        Language::SelfMethodAccess(message, _parameters) => {
            let callee_type = scope.get_self();

            scope.find_type_method_type(&callee_type, message)
        }
        Language::SelfAttributeAccess(message) => {
            let callee = scope.get_self();

            vec![scope
                .find_type_attribute_type(&callee, message, scope)
                .unwrap()]
        }
        Language::TypeInstance(name, _named_types, _values) => {
            let attributes = scope.find_type_attribute(name);

            let types = attributes
                .iter()
                .map(|(_name, kind)| kind.clone())
                .collect();

            vec![ValueType::CustomType(name.clone(), types)]
        }
    }
}

pub fn build_function_key(function_name: &str, params: &Params) -> String {
    let value_types = params
        .into_iter()
        .map(|(_, value_type)| value_type.clone())
        .collect::<Vec<ValueType>>();

    function_key(function_name, &value_types)
}

pub fn function_key(function_name: &str, value_types: &Vec<ValueType>) -> String {
    if value_types.is_empty() {
        return function_name.to_owned();
    }

    let value_types_str = value_types
        .into_iter()
        .map(|value_type| format!("{}", value_type))
        .collect::<Vec<String>>()
        .join("_");

    format!("{}_{}", function_name, value_types_str)
}

fn build_ast(
    pair: Pair<Rule>,
    scope: &mut Context,
    modules: &mut Modules,
) -> Result<Language, Error<Rule>> {
    match pair.as_rule() {
        Rule::bool => {
            let value = pair.as_str();

            if value == "true" {
                Ok(Language::Boolean(true))
            } else {
                Ok(Language::Boolean(false))
            }
        }
        Rule::import => {
            let mut inner = pair.into_inner();

            let path = inner.next().unwrap().as_str();

            let module = to_ast_from_file(path)?;

            if let Language::Module(
                _module_name,
                _functions,
                _instructions,
                exports,
                _imports,
                _types,
            ) = &module
            {
                for export in exports {
                    match export {
                        Export::Function(function_name, params, returns) => {
                            let name_key = build_function_key(&function_name, &params);

                            scope.add_variable(format!("{}", name_key), returns.clone());
                        }
                    }
                }
            }

            modules.push(module);

            Ok(Language::Import(path.to_string()))
        }
        Rule::float => {
            let value = pair.as_str();

            Ok(Language::Float(value.to_owned()))
        }
        Rule::integer => {
            let value = pair.as_str().parse().unwrap();

            Ok(Language::Number(value))
        }
        Rule::variable => {
            let name = pair.as_str().to_string();

            if let Some(kind) = scope.find_variable(&name) {
                Ok(Language::Variable(name, kind.clone()))
            } else {
                panic!("variable: ${} not found", name)
            }
        }
        Rule::variable_def => {
            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str().to_string();
            let kind = inner.next().unwrap().as_str();

            let value_type = str_to_value_type(kind);

            scope.add_variable(name.to_string(), vec![value_type.clone()]);

            Ok(Language::Variable(name, vec![value_type]))
        }
        Rule::unary => parse_instruction(&mut pair.into_inner(), scope, modules),
        Rule::boolean => parse_instruction(&mut pair.into_inner(), scope, modules),
        Rule::expression => parse_instruction(&mut pair.into_inner(), scope, modules),
        Rule::summand => parse_instruction(&mut pair.into_inner(), scope, modules),
        Rule::instruction => parse_instruction(&mut pair.into_inner(), scope, modules),
        Rule::primary => {
            let mut inner = pair.into_inner();

            build_ast(inner.next().unwrap(), scope, modules)
        }
        Rule::function_def => {
            let mut visibility = Visiblitity::Private;
            let mut results = vec![];
            let mut params = vec![];
            let mut instructions = vec![];

            scope.add_new_scope();

            let mut inner = pair.into_inner();

            let mut elem = inner.next().unwrap();

            if elem.as_rule() == Rule::public {
                visibility = Visiblitity::Public;

                elem = inner.next().unwrap();
            }

            let function_name = elem.as_str().to_string();
            elem = inner.next().unwrap();

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

                    scope.add_variable(name.to_string(), vec![kind]);
                }

                elem = inner.next().unwrap();
            }

            if elem.as_rule() == Rule::results {
                let mut pair = elem.into_inner();

                while let Some(rule) = pair.next() {
                    let kind = str_to_value_type(rule.as_str());

                    results.push(kind);
                }

                elem = inner.next().unwrap();
            }

            if elem.as_rule() == Rule::block {
                let mut pair = elem.into_inner();

                while let Some(instruction) = pair.next() {
                    instructions.push(build_ast(instruction, scope, modules)?);
                }
            }

            scope.destroy_scope();

            // function get's added on it's parent scope
            let name_key = build_function_key(&function_name, &params);
            scope.add_variable(name_key, results.clone());

            Ok(Language::Function(
                function_name,
                params,
                results,
                instructions,
                visibility.clone(),
            ))
        }
        Rule::block => {
            let mut instructions = vec![];
            let mut inner = pair.into_inner();
            scope.add_new_scope();

            while let Some(instruction) = inner.next() {
                instructions.push(build_ast(instruction, scope, modules)?);
            }

            scope.destroy_scope();

            Ok(Language::Block(instructions))
        }
        Rule::function_call => {
            let mut inner = pair.into_inner();

            let function_name = inner.next().unwrap().as_str().to_string();

            let mut params = vec![];

            if let Some(pair_params) = inner.next() {
                let mut params_inner = pair_params.into_inner();

                while let Some(param) = params_inner.next() {
                    params.push(build_ast(param, scope, modules)?);
                }
            }

            let param_types = params
                .iter()
                .map(|param| find_value_type(param, scope))
                .flatten()
                .collect::<Vec<ValueType>>();

            let name_key = function_key(&function_name, &param_types);

            if let Some(value_type) = scope.find_variable(&name_key) {
                Ok(Language::Call(function_name, params, value_type.clone()))
            } else {
                println!("variables: {:?}", scope.variables);
                println!("modules: {:?}", modules);

                panic!("No variable found {}", name_key)
            }
        }
        Rule::module => {
            let mut instructions = vec![];
            let mut functions = vec![];
            let mut exports = vec![];
            let mut imports = vec![];
            let mut types = Types::new();

            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str().to_string();

            let block = inner.next().unwrap();

            let mut pair = block.into_inner();

            while let Some(instruction) = pair.next() {
                let ast = build_ast(instruction, scope, modules)?;

                match &ast {
                    Language::Function(name, params, returns, _, visibility) => {
                        match visibility {
                            Visiblitity::Public => exports.push(Export::Function(
                                name.clone(),
                                params.clone(),
                                returns.clone(),
                            )),
                            Visiblitity::Private => (),
                        }

                        functions.push(ast);
                    }
                    Language::Import(filepath) => imports.push(filepath.to_string()),
                    Language::CustomType(name, _named_types, attributes, functions) => {
                        scope.add_type_attributes(name, attributes);

                        for function in functions {
                            if let Language::Function(
                                function_name,
                                _params,
                                results,
                                _instructions,
                                _visibility,
                            ) = function
                            {
                                scope.add_type_methods(name, function_name, results);
                            }
                        }

                        types.push(ast);
                    }
                    _ => instructions.push(ast),
                }
            }

            Ok(Language::Module(
                name,
                functions,
                instructions,
                exports,
                imports,
                types,
            ))
        }
        Rule::symbol => {
            let mut inner = pair.into_inner();

            let string = inner.next().unwrap();

            Ok(Language::Symbol(string.as_str().to_string()))
        }
        Rule::conditional => {
            let mut inner = pair.into_inner();

            let kind = match inner.next().unwrap().as_str() {
                "if" => ConditionalType::If,
                "unless" => ConditionalType::Unless,
                x => panic!("{} not supported", x),
            };

            let instruction = build_ast(inner.next().unwrap(), scope, modules)?;
            let block = Box::new(build_ast(inner.next().unwrap(), scope, modules)?);

            let block2 = if let Some(else_block) = inner.next() {
                Some(Box::new(build_ast(else_block, scope, modules)?))
            } else {
                None
            };

            Ok(Language::Conditional(
                kind,
                Box::new(instruction),
                block,
                block2,
            ))
        }
        Rule::assignation => {
            let mut inner = pair.into_inner();

            let left_pair = inner.next().unwrap();

            let right = build_ast(inner.next().unwrap(), scope, modules)?;

            let left = match left_pair.as_rule() {
                Rule::variable_def => build_ast(left_pair, scope, modules)?,
                Rule::variable => {
                    let name = left_pair.as_str().to_string();

                    Language::Variable(name, find_value_type(&right, scope))
                }
                _ => panic!("Left part of assignation must be a variable"),
            };

            if let Language::Variable(name, types) = &left {
                scope.add_variable(name.clone(), types.clone());
            }

            Ok(Language::Infix(
                Operation::Assignment,
                Box::new(left),
                Box::new(right),
            ))
        }
        Rule::array => {
            let mut instructions = vec![];
            let mut inner = pair.into_inner();

            while let Some(instruction) = inner.next() {
                instructions.push(build_ast(instruction, scope, modules)?);
            }

            Ok(Language::Array(instructions))
        }
        Rule::array_access => {
            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str();
            let index = inner.next().unwrap().as_str();

            Ok(Language::ArrayAccess(
                name.to_string(),
                index.parse().unwrap(),
            ))
        }
        Rule::self_attribute_access => {
            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str();

            Ok(Language::SelfAttributeAccess(name.to_string()))
        }
        Rule::variable_attribute_access => {
            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str();
            let method = inner.next().unwrap().as_str();

            Ok(Language::TypeAttributeAccess(
                name.to_string(),
                method.to_string(),
            ))
        }
        Rule::variable_method_call => {
            let mut self_function_call = pair.into_inner();

            let name = self_function_call.next().unwrap().as_str();
            let pair = self_function_call.next().unwrap();

            let mut function_call = pair.into_inner();

            let function_name = function_call.next().unwrap().as_str().to_string();

            let mut params = vec![];

            if let Some(pair_params) = function_call.next() {
                let mut params_inner = pair_params.into_inner();

                while let Some(param) = params_inner.next() {
                    params.push(build_ast(param, scope, modules)?);
                }
            }

            Ok(Language::TypeCall(name.to_string(), function_name, params))
        }
        Rule::self_function_call => {
            let mut self_function_call = pair.into_inner();
            let pair = self_function_call.next().unwrap();

            let mut function_call = pair.into_inner();

            let function_name = function_call.next().unwrap().as_str().to_string();

            let mut params = vec![];

            if let Some(pair_params) = function_call.next() {
                let mut params_inner = pair_params.into_inner();

                while let Some(param) = params_inner.next() {
                    params.push(build_ast(param, scope, modules)?);
                }
            }

            Ok(Language::SelfMethodAccess(function_name, params))
        }
        Rule::custom_type => {
            let mut attributes = Attributes::new();
            let mut methods = vec![];

            let mut inner = pair.into_inner();

            let named_types = NamedTypes::new();
            let name = inner.next().unwrap().as_str();

            if let Some(attributes_rule) = inner.next() {
                let mut attributes_inner = attributes_rule.into_inner();

                while let Some(attribute) = attributes_inner.next() {
                    let mut attribute_inner = attribute.into_inner();

                    if let Some(name) = attribute_inner.next() {
                        let kind = attribute_inner.next().unwrap().as_str();

                        attributes.insert(name.as_str().to_string(), str_to_value_type(kind));
                    }
                }
            }

            if let Some(methods_rule) = inner.next() {
                let mut methods_inner = methods_rule.into_inner();

                while let Some(method) = methods_inner.next() {
                    methods.push(build_ast(method, scope, modules)?);
                }
            }

            Ok(Language::CustomType(
                name.to_string(),
                named_types,
                attributes,
                methods,
            ))
        }
        Rule::custom_type_instance => {
            let mut inner = pair.into_inner();

            let name = inner.next().unwrap().as_str();

            let mut values = IndexMap::new();
            let named_types = NamedTypes::new();

            if let Some(attributes) = inner.next() {
                let mut attributes_inner = attributes.into_inner();

                while let Some(attribute) = attributes_inner.next() {
                    let instruction = attributes_inner.next().unwrap();

                    values.insert(
                        attribute.as_str().to_string(),
                        build_ast(instruction, scope, modules)?,
                    );
                }
            }

            Ok(Language::TypeInstance(
                name.to_string(),
                named_types,
                values,
            ))
        }
        x => panic!("No rule match: {:?}", x),
    }
}

fn str_to_value_type(value_type: &str) -> ValueType {
    match value_type {
        "Int" => ValueType::Integer,
        "Float" => ValueType::Float,
        "Symbol" => ValueType::Symbol,
        "i32" => ValueType::Native(Native::i32),
        "i64" => ValueType::Native(Native::i64),
        _kind => ValueType::Native(Native::i32), // illegal
    }
}

pub fn to_ast(original: &str) -> Result<Language, Error<Rule>> {
    let mut block = vec![];
    let mut modules = vec![];
    let mut scope = Context::default();

    match UniversalParser::parse(Rule::language, original) {
        Ok(pairs) => {
            let mut pair = pairs.into_iter();

            while let Some(inner_pair) = pair.next() {
                match build_ast(inner_pair, &mut scope, &mut modules) {
                    Ok(ast) => block.push(ast),
                    Err(e) => {
                        println!("{}", e);

                        panic!(e)
                    }
                }
            }
        }
        Err(e) => return Err(e),
    }

    if modules.len() > 0 {
        return Ok(Language::Program([modules, block].concat()));
    }

    if block.len() == 1 {
        Ok(block[0].to_owned())
    } else {
        Ok(Language::Block(block))
    }
}

pub fn to_ast_from_file(filepath: &str) -> Result<Language, Error<Rule>> {
    to_ast(&load_file(&filepath))
}

fn parse_instruction(
    inner: &mut Pairs<Rule>,
    scope: &mut Context,
    modules: &mut Modules,
) -> Result<Language, Error<Rule>> {
    let summand = build_ast(inner.next().unwrap(), scope, modules)?;

    match inner.next() {
        Some(operator) => match inner.next() {
            Some(pair) => {
                let summand2 = build_ast(pair, scope, modules)?;

                let instruction = Language::Infix(
                    str_operator_to_enum(operator.as_str()),
                    Box::new(summand),
                    Box::new(summand2),
                );

                match inner.next() {
                    Some(second_operator) => Ok(Language::Infix(
                        str_operator_to_enum(second_operator.as_str()),
                        Box::new(instruction),
                        Box::new(parse_instruction(inner, scope, modules)?),
                    )),
                    None => Ok(instruction),
                }
            }
            None => panic!("seccond summand missing"),
        },
        None => Ok(summand),
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
        op => Operation::Native(op.to_string()),
    }
}

pub fn load_file(filepath: &str) -> String {
    let file = fs::read_to_string(format!("{}.star", filepath))
        .expect("Something went wrong reading the file");

    let filename = Path::new(filepath).file_stem().unwrap().to_str().unwrap();

    format!("module {}\n{} end", filename, file)
}

#[cfg(test)]
mod test {
    use super::*;
    use Language::*;

    #[test]
    fn modules() {
        let exports = vec![];
        let imports = vec![];
        let types = vec![];

        let module = to_ast(
            "module awesome
            42
        end
        ",
        );

        let expected = Module(
            "awesome".to_string(),
            vec![],
            vec![Number(42)],
            exports,
            imports,
            types,
        );

        assert_eq!(module, Ok(expected));

        let module = to_ast(
            "module awesome
            fn tres(): Int
                3
            end
        end",
        );

        let function = Function(
            "tres".to_string(),
            vec![],
            vec![ValueType::Integer],
            vec![Language::Number(3)],
            Visiblitity::Private,
        );

        let exports = vec![];
        let imports = vec![];
        let types = vec![];
        let expected = Language::Module(
            "awesome".to_string(),
            vec![function],
            vec![],
            exports,
            imports,
            types,
        );

        assert_eq!(module, Ok(expected));

        let module = to_ast(
            "module awesome
            export fn tres(): Int
                3
            end
        end",
        );

        let function = Function(
            "tres".to_string(),
            vec![],
            vec![ValueType::Integer],
            vec![Language::Number(3)],
            Visiblitity::Public,
        );

        let exports = vec![Export::Function(
            "tres".to_string(),
            vec![],
            vec![ValueType::Integer],
        )];
        let imports = vec![];
        let types = vec![];
        let expected = Language::Module(
            "awesome".to_string(),
            vec![function],
            vec![],
            exports,
            imports,
            types,
        );

        assert_eq!(module, Ok(expected));
    }

    #[test]
    fn constants() {
        assert_eq!(to_ast("42"), Ok(Number(42)));

        assert_eq!(
            to_ast(
                "
                42
                ",
            ),
            Ok(Number(42))
        );
    }

    #[test]
    fn variables() {
        let program = "
            module test
              x = 5
              x
            end
            ";

        let ast = to_ast(program).expect("error loading program");

        let instruction = match ast {
            Module(_, _, instructions, _, _, _types) => instructions
                .last()
                .expect("no instructions on module")
                .clone(),
            _ => unreachable!(),
        };

        assert_eq!(
            instruction,
            Variable("x".to_string(), vec![ValueType::Native(Native::i32)])
        );
    }

    #[test]
    fn infix() {
        let instruction = "1 + 2 - 3";
        let expected = Infix(
            Operation::Minus,
            Box::new(Infix(
                Operation::Add,
                Box::new(Number(1)),
                Box::new(Number(2)),
            )),
            Box::new(Number(3)),
        );

        assert_eq!(to_ast(instruction), Ok(expected))
    }

    #[test]
    fn function_call() {
        let program = "
            module test
              fn tres(): i32
                  3
              end

              fn add(x: i32, y: i32): i32
                x + y
              end

              tres()
              add(1, 2)
            end
            ";

        let ast = to_ast(program).expect("error loading program");

        let instructions = match ast {
            Module(_, _, instructions, _, _, _types) => instructions,
            _ => unreachable!(),
        };

        assert_eq!(
            instructions,
            vec![
                Call(
                    "tres".to_string(),
                    vec![],
                    vec![ValueType::Native(Native::i32)]
                ),
                Call(
                    "add".to_string(),
                    vec![Number(1), Number(2)],
                    vec![ValueType::Native(Native::i32)]
                ),
            ]
        );
    }

    #[test]
    fn operations() {
        assert_eq!(
            to_ast("5 + 2"),
            Ok(Infix(
                Operation::Add,
                Box::new(Number(5)),
                Box::new(Number(2))
            ))
        );

        let program = "
            module test
              x = 5
              x + 1
            end
            ";

        let ast = to_ast(program).expect("error loading program");

        let instruction = match ast {
            Module(_, _, instructions, _, _, _types) => instructions
                .last()
                .expect("no instructions on module")
                .clone(),
            _ => unreachable!(),
        };

        assert_eq!(
            instruction,
            Infix(
                Operation::Add,
                Box::new(Variable(
                    "x".to_string(),
                    vec![ValueType::Native(Native::i32)]
                )),
                Box::new(Number(1))
            )
        );
    }

    #[test]
    fn assignations() {
        assert_eq!(
            to_ast("x: i32 = 1"),
            Ok(Infix(
                Operation::Assignment,
                Box::new(Variable(
                    "x".to_string(),
                    vec![ValueType::Native(Native::i32)]
                )),
                Box::new(Number(1))
            ))
        );

        assert_eq!(
            to_ast("x = 1"),
            Ok(Infix(
                Operation::Assignment,
                Box::new(Variable(
                    "x".to_string(),
                    vec![ValueType::Native(Native::i32)]
                )),
                Box::new(Number(1))
            ))
        );
    }

    #[test]
    fn functions() {
        let result = to_ast(
            "fn hello: Int
                42
            end",
        );

        let expected = Function(
            "hello".to_string(),
            vec![],
            vec![ValueType::Integer],
            vec![Number(42)],
            Visiblitity::Private,
        );

        assert_eq!(result, Ok(expected));

        let result = to_ast(
            "fn hello(num: Int): Int
               42
             end",
        );

        let expected = Function(
            "hello".to_string(),
            vec![("num".to_string(), ValueType::Integer)],
            vec![ValueType::Integer],
            vec![Number(42)],
            Visiblitity::Private,
        );

        assert_eq!(result, Ok(expected));

        let result = to_ast(
            "fn hello(num: Int): Int
              num
             end",
        );

        let expected = Function(
            "hello".to_string(),
            vec![("num".to_string(), ValueType::Integer)],
            vec![ValueType::Integer],
            vec![Variable("num".to_string(), vec![ValueType::Integer])],
            Visiblitity::Private,
        );

        assert_eq!(result, Ok(expected));

        let result = to_ast(
            "fn hello(num: Int): Int
               num + 2
             end",
        );

        let expected = Function(
            "hello".to_string(),
            vec![("num".to_string(), ValueType::Integer)],
            vec![ValueType::Integer],
            vec![Infix(
                Operation::Add,
                Box::new(Variable("num".to_string(), vec![ValueType::Integer])),
                Box::new(Number(2)),
            )],
            Visiblitity::Private,
        );

        assert_eq!(result, Ok(expected));

        let result = to_ast(
            "fn add(num: Int, num2: Int): Int
               num + num2
             end",
        );

        let expected = Function(
            "add".to_string(),
            vec![
                ("num".to_string(), ValueType::Integer),
                ("num2".to_string(), ValueType::Integer),
            ],
            vec![ValueType::Integer],
            vec![Infix(
                Operation::Add,
                Box::new(Variable("num".to_string(), vec![ValueType::Integer])),
                Box::new(Variable("num2".to_string(), vec![ValueType::Integer])),
            )],
            Visiblitity::Private,
        );

        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn strings() {
        assert_eq!(to_ast(":42"), Ok(Symbol("42".to_string())));
        assert_eq!(
            to_ast(":\"steven barragan\""),
            Ok(Symbol("steven barragan".to_string()))
        );
    }

    #[test]
    fn conditionals_if() {
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
            None,
        );

        assert_eq!(to_ast(program), Ok(expected));
    }

    #[test]
    fn conditionals_unless() {
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
            None,
        );

        assert_eq!(to_ast(program), Ok(expected));
    }

    #[test]
    fn conditionals_if_else() {
        let program = "
            if 1
              1
            else
              2
            end
        ";

        let instructions_if = vec![Number(1)];
        let instructions_else = vec![Number(2)];

        let expected = Conditional(
            ConditionalType::If,
            Box::new(Number(1)),
            Box::new(Language::Block(instructions_if)),
            Some(Box::new(Language::Block(instructions_else))),
        );

        assert_eq!(to_ast(program), Ok(expected));

        let program = "
            if 1 <= 2
              1
            else
              2
            end
        ";

        let instructions_if = vec![Number(1)];
        let instructions_else = vec![Number(2)];
        let condition = Infix(
            Operation::LessThanOrEq,
            Box::new(Number(1)),
            Box::new(Number(2)),
        );

        let expected = Conditional(
            ConditionalType::If,
            Box::new(condition),
            Box::new(Language::Block(instructions_if)),
            Some(Box::new(Language::Block(instructions_else))),
        );

        assert_eq!(to_ast(program), Ok(expected));
    }

    #[test]
    fn conditionals_if_eq() {
        let program = "
            if 1 == 1
              1
            end
            ";

        let expected = Conditional(
            ConditionalType::If,
            Box::new(Infix(
                Operation::Equal,
                Box::new(Number(1)),
                Box::new(Number(1)),
            )),
            Box::new(Language::Block(vec![Number(1)])),
            None,
        );

        assert_eq!(to_ast(program), Ok(expected));
    }

    #[test]
    fn booleans() {
        let program = "true";
        let expected = Boolean(true);

        assert_eq!(to_ast(program), Ok(expected))
    }

    #[test]
    fn floats() {
        let program = "2.23";
        let expected = Float("2.23".to_owned());

        assert_eq!(to_ast(program), Ok(expected))
    }

    #[test]
    fn imports() {
        let program = "import test";
        let expected = Program(vec![
            Module(
                "test".to_string(),
                vec![Function(
                    "hello".to_string(),
                    vec![],
                    vec![ValueType::Integer],
                    vec![Number(42)],
                    Visiblitity::Public,
                )],
                vec![],
                vec![Export::Function(
                    "hello".to_string(),
                    vec![],
                    vec![ValueType::Integer],
                )],
                vec![],
                vec![],
            ),
            Import("test".to_string()),
        ]);

        assert_eq!(to_ast(program), Ok(expected))
    }

    #[test]
    fn program() {
        let program = "
            module main
                import test

                hello()
            end
        ";

        let types = Types::new();

        let function = Function(
            "hello".to_string(),
            vec![],
            vec![ValueType::Integer],
            vec![Number(42)],
            Visiblitity::Public,
        );
        let export = Export::Function("hello".to_string(), vec![], vec![ValueType::Integer]);
        let test_module = Module(
            "test".to_string(),
            vec![function],
            vec![],
            vec![export],
            vec![],
            types,
        );

        let instruction = Call("hello".to_string(), vec![], vec![ValueType::Integer]);
        let types = Types::new();
        let main_module = Module(
            "main".to_string(),
            vec![],
            vec![instruction],
            vec![],
            vec!["test".to_string()],
            types,
        );

        let expected = Program(vec![test_module, main_module]);

        assert_eq!(to_ast(program), Ok(expected));
    }

    #[test]
    fn arrays() {
        let program = "[]";
        let expected = Array(vec![]);

        assert_eq!(to_ast(program), Ok(expected));

        let program = "a = []";
        let array_type = ValueType::Array(vec![ValueType::Integer]);
        let expected = Infix(
            Operation::Assignment,
            Box::new(Variable("a".to_string(), vec![array_type])),
            Box::new(Array(vec![])),
        );

        assert_eq!(to_ast(program), Ok(expected));

        let program = "[1]";
        let expected = Array(vec![Number(1)]);

        assert_eq!(to_ast(program), Ok(expected));

        let program = "a = [1,2]";
        let array_type = ValueType::Array(vec![ValueType::Native(Native::i32)]);
        let expected = Infix(
            Operation::Assignment,
            Box::new(Variable("a".to_string(), vec![array_type])),
            Box::new(Array(vec![Number(1), Number(2)])),
        );
        assert_eq!(to_ast(program), Ok(expected));

        let program = "
           a = [1]
           a[0]
        ";
        let array_type = ValueType::Array(vec![ValueType::Native(Native::i32)]);
        let expected = Block(vec![
            Infix(
                Operation::Assignment,
                Box::new(Variable("a".to_string(), vec![array_type])),
                Box::new(Array(vec![Number(1)])),
            ),
            ArrayAccess("a".to_string(), 0),
        ]);
        assert_eq!(to_ast(program), Ok(expected));
    }

    #[test]
    fn types_definition() {
        let program = "
        Type People
          age: i32

          fn calculate(): i32
            self.age
          end

          fn age(): i32
            self.calculate()
          end
        end
        ";

        let named_types = NamedTypes::new();

        let mut attributes = Attributes::new();
        attributes.insert("age".to_string(), ValueType::Native(Native::i32));

        let function_calculate = Function(
            "calculate".to_owned(),
            Params::new(),
            vec![ValueType::Native(Native::i32)],
            vec![SelfAttributeAccess("age".to_string())],
            Visiblitity::Private,
        );

        let function_age = Function(
            "age".to_owned(),
            Params::new(),
            vec![ValueType::Native(Native::i32)],
            vec![SelfMethodAccess("calculate".to_string(), Parameters::new())],
            Visiblitity::Private,
        );

        let expected = CustomType(
            "People".to_owned(),
            named_types,
            attributes,
            vec![function_calculate, function_age],
        );

        assert_eq!(to_ast(program), Ok(expected));
    }

    #[test]
    fn types_instances() {
        let program = "
        module Test
            Type Person
              level: i32
            end

            Person
              level: 5
            end
        end
        ";

        let named_types = NamedTypes::new();

        let mut attributes = Attributes::new();
        attributes.insert("level".to_string(), ValueType::Native(Native::i32));

        let custom_type = CustomType("Person".to_owned(), named_types, attributes, vec![]);

        let mut person_values = IndexMap::new();
        person_values.insert("level".to_string(), Number(5));

        let named_types = NamedTypes::new();
        let instruction = TypeInstance("Person".to_string(), named_types, person_values);

        let expected = Module(
            "Test".to_string(),
            vec![],
            vec![instruction],
            vec![],
            vec![],
            vec![custom_type],
        );

        assert_eq!(to_ast(program), Ok(expected));
    }
}
