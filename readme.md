Universal
=========

Universal is a multi-syntax compiler. Developers will be able to pick a syntax they feel more comfortable and familiar with.
It compiles to webassembly with very minimal overhead allowing it to run securely and pretty much everywhere. 
Ruby developers will be able to work in the exact same codebase as Typescript and Rust ones.

Features:

- Statically typed (with auto type inference)
- Implicit returns
- Multiple returns
- Immutable data structures
- Pattern matching
- Pipe operator
- Function overload
- No garbage collector
- Language server protocol
- Auto formatting
- Unit testing
- Runs everywhere
- Import wasm modules

Universal doesn't pretend to support all features from each language but define a common ground for all of them.
It won't be limited to mirror existing syntaxes but it'll support new ones as well.

rust like
```
pub fn factorial(number: Int) -> Int {
  let limit = 1;

  if number <= limit {
    1
  } else {
    number * factorial(number - 1)
  }
}
```

ruby like
```
export def factorial(number: Int) : Int
  limit = 1

  if number <= limit
    1
  else
    number * factorial(number - 1)
  end
end
```

typescript like
```
export function factorial(number: Int) -> Int {
  let limit = 1;

  if (number <= limit) {
    1
  } else {
    number * factorial(number - 1)
  }
}
```

Disclaimer: Universal is research software under development and still in its pre-alpha phase.
