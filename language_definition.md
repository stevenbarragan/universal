# Pipe Operator
```
  10   | it           is 10
  10   | it > 2       is true
  10   | double       is 20
  10   | double | inc is 21
  10 1 | add          is 11
  10   | 42           is 42

  human = Human { age: 30 }
  human | age eq 30 is true
```

# Types

## Int
```
42    is type Int
55gb  is type Int
21sec is type Int
```

## Float
```
1.0
3.42 is type Float
PI   is type Float
```

## Bool
```
true     is type bool
false    is not true
not true is false

1    eq     1     is true
1    not eq 1     is false
true and    false is false
true or     false is true
```

## String
```
"square"          is type String
square            is type String
square            is "square"

name = Steven
"Hello { name }" is "Hello Steven"
```

## Array
```
[]      is empty
[1 2 3] is type Array

numbers  = [1 2 3]              <<--------------------------- each array element in different color!

numbers | first        is 1
numbres | nth 0        is 1
numbers | size         is 3

numbers | where it > 1 is [2 3]
numbers | insert 4     is [1 2 3 4]

[head ..tail] = [1 2 3]                <<---------------------- better syntax for this!!
head is 1
tail is [2 3]
```

## Range
```
1..3   is [1 2 3]
1..<3  is [1 2]
1..    is [1 ..rest]         <<-------------------------------- pattern matching? infinity?
..3    is [1 2 3]
```

## Custom types
```
type Pet
  name: String
  age: Int
end

nemo = Pet { name: Nemo, age: 8 }
blue = Pet { Blue 9 }

my_pets = [nemo dori]

my_pets | insert kato 11 | where age > 10 | first is Pet { name: kato age: 11 }

type Human
  name: String
  pets: Pet[]
  numbers: Int[]
end

jonh   = Human { name: Jonh pets: [nemo] }
steven = Human { Steven [dory] }

humans = [jonh steven]

humans | where pets.name == dori | first is steven
humans | pets | where name == dory | first is steven <--------- leaning towards this sintax
humans | (pets | where name == dori) | first is steven

humans | get pets | where name == dory is dory   <------------- along with this one 
humans | pets | where name == dory | first is dory

humans | first | where numbers.it > 24
humans | first | numbers               | where it > 24

humans | first | name == Steven and { pets | include? dory } is false 
```

## Enums
```
enum IpVersion
  V4
  V6
end

four = IpVersion::V4
six  = IpVersion | V6

four is IpVersion | V4

42 | it < 14 and it > 20 is false

route(ip_kind: IpAddres)
  if ip_kind eq IpVersion | v4
    ...
end

type IpAddress
  version: IpVersion
  address: String
end

home = IpAddres
  version: IpVersion | V4
  address: 127.0.0.1
end

home | version is IpVersion | v4

IpVersion | V6 vs IpVersion::V6 <------------------------------ enum access? leaning towars second version, imports uses same sintax, what about both

work = IpAddress { IpVersion | v6 2001:db8:0:0:0:ff00:42:8329 }
work = IpAddress { IpVersion::v6 2001:db8:0:0:0:ff00:42:8329 }

adresses = [home work]
adresses | where version eq V4 | first is home
```

## Others
```
  1min is 60sec and type Int

  60sec is 1min
  1day  is 24hrs
  1wk   is 7days

  8bit is 1b
  1kb  is 1024b

  127.0.0.1 is type String
```

# Identifier
```
a = 1                <<---------------------------------------- in-mutable?

a | inc is 2
a is 1
```

# Variables
```
mut x := 1                 <<---------------------------------- mutable?
mut x = 1                  <<---------------------------------- other option
x := 1
x = 1

x := 2

x | inc!             <<---------------------------------------- bang methods for mutables
x is 26

for it in 1..3
  it | print
end
```

# Conditionals
```
if 42 | it > 22 and it < 50
  "be happy"
else if not true
  "be happy"
else
  "be happy"
end

true ? verdad : falso is verdad

walk() if age > 1  <------------------------------------------- use cases? early return. Hold on this syntax?

true and true is true
true && true  is true                <<------------------------ and vs &&

41 | it > 30 and it < 50 is true

human = Human { Steven 30 }
human | age >= 21 and type? Human    is true 
human | age >= 21 and fly?           is false 
human | age >= 21 and ( it | fly? )  is false 
```

## Functions
```
# Fibonacci Sequence
fib(num: Int)
  num <= 1 ? num : fib(num - 1) + fib(num - 2) <---------------- we need parentesis calls after all?!
fib(numbers: Int[])
  numbers | map fib
check
  0     | fib is 0
  3     | fib is 5
  [1 2] | fib is [1 3]
end

one-two()
  1 2
check
  one-two() is 1 2
  | one-two is 1 2 <------------------------------------------- execute function without params using pipe!?
end

a b = one-two()
a b = | one-two

# product and addition
pro-addition(a: Int b: Int)
  a * b, a + b <----------------------------------------------- how to multiple returns?
check
  1 2 | pro-addition is 2 3
  3 5 | pro-addition is [8 15] <------------------------------- multiple responses can be treated as an array? if they're the same type? too complicated!
end
```

# Lambdas
```
1 | it              is 1
1 | (it) it + 1     is 2
1 | it + 1          is 2

inc = it + 1 <------------------------------------------------- do not allow lambda assignation?!
inc(it) it + 1

1 | inc is 2

inc is type (Int): Int

10    | it * it is 20
10 20 | a + b   is 30

[1 2] | map    x + 1    is [2 3]
[1 2] | map    double   is [2 4]
[1 2] | reduce it + acc is 3

[nemo dory] | map double raises Error { no-method double(Pet) }
```

# Generic types
```
map(iterator: any[] action(any): kind) kind[] <------------- annotated lambdas
  mut result = kind[] <--------------------------------------- mutable?! :( 
  for elem in iterator
    result = result | insert action(elem)
    result |= insert action(elem) <--------------------------- syntax sugar for pipe reassigment? pipement?
  end

  result
end
```

# Loops
```
for x in 0..42
  x | double | print
end

0..3 | map double is [0 2 4 6]
0..3 | reduce x + sum is 6
0..3 | max is 3

loop
  "to the infinity" | print
end
```

# Pattern matching
```
```

# Imports/Includes
```
import standard-library as std

"hello world" | std::print

include standard-library
include std              # after being imported

"hello world" | print
```

# Tests
```
check
  1 | double is 2

  tesla = Car { 21 tesla }
  cars = [tesla other-car more-cars]
  cars | find id == 21 is tesla and it.name is "tesla"

  [1] is not empty and it is type [Int]  << ------------------- booleans on test? | it is [1]

  1 0 | div raises Error { aritmetic division-by-zero }
end
```

# Examples
```
"hello world" | print

# FizzBuzz
fizzbuzz(x: Int)
  if x mod 15 eq 0
    FizzBuzz
  else if x mod 3 eq 0
    Fizz
  else if x mod 5 eq 0
    Buzz
  else
    x
  end
check
  51 | fizzbuzz is 51
  9  | fizzbuzz is Fizz
  20 | fizzbuzz is Buzz
  45 | fizzbuzz is FizzBuzz
end

..100 | map fizzbuzz | print

..10 | where it | fizzbuzz eq Fizz is [3 9] <---------------------- How to call regular functions to perform a sql query?

# Quicksort
quicksort(list: sortable[]) <<-------------------------------------- Array<Any> | any type of array | tag sortable | tag is no regessary
  return list if list | empty?

  pivot = list | first
  left  = list | where it < pivot  | quicksort
  right = list | where it >= pivot | quicksort

  left | join right
check
  []         | quicksort is []
  [2 1]      | quicksort is [1 2]
  [42 2 1 5] | quicksort is [1 2 5 42]
end

# Combines all list elements using action
reduce(list: any[] init: result action(any result): result)
  return init if list | empty?

  mut acc = init 
  for elem in list
    acc = elem | action acc
  end
  
  acc
reduce(list: any[] action(any result): result)
  raise Error { expected non-empty-array } if list | empty?

  init = list | first

  list | tail | reduce init action
check
  [1 2 3] | reduce it + acc    is 6
  []      | reduce 20 it + acc is 20
  [1 2 3] | reduce 20 it + acc is 26
end
```
