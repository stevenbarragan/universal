use crate::ast::*;

pub fn size(value_types: &Vec<ValueType>) -> usize {
    value_types
        .into_iter()
        .map(|value_type| match value_type {
            ValueType::Bool => 1,
            ValueType::Float => 4,
            ValueType::Integer => 4,
            ValueType::Native(native) => match native {
                Native::i32 => 4,
                Native::i64 => 8,
            },
            ValueType::Symbol => 8,
            ValueType::Array(_) => 4,
            ValueType::CustomType(_name, types) => size(types),
        })
        .sum()
}
