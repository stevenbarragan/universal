

pub fn size(value_types: &Vec<ValueType>) -> usize {
    value_types.into_iter()
        .map(|value_type| match value_type {
            ValueType::Bool => 1,
            ValueType::Float => 1,
            ValueType::Integer => 4,
            ValueType::Native(_name) => 1, // fix me!
            ValueType::Symbol => 8,
            ValueType::Array(_) => 4,
            ValueType::CustomType(types) => size(types)
        })
        .sum()
}
