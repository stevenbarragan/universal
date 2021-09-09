# Pipe Operator
```
  5   | double is double(5)
  1 2 | add    is add(1, 3)
  5   | double is 10
  10  | it > 5 is true

  Day { name: Monday } | name == Monday is true

  human | age >= 21 and fly? is false <------------------------ implicit params!
  human | age >= 21 and (it | fly?) is false 
  human | age >= 21 and fly?(it) is false 

  a = 5
  5 | it + a is 10 <------------------------------------------- should it have access to "a"?
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

not true is false <<------------------------------------------ how are subfixes gonna look like?
!true    is false

1    ==  1     is true
1    eq  1     is true
true and false is false
true or  false is true
true xor true  is false

true not eq false is true <<---------------------------------- how is it on wast?
true !=     false is true
```
## String
```
"square"          is type String
square            is type String
square            is "square"
"Hello { world }" is "Hello world"
```

## Array
```
[]      is empty
[1 2 3] is type Array

numbers  = [1 2 3]              <<--------------------------- each array element in different color!

numbers[0]      is 1                   <<--------------------- we can hold on this syntax?
numbers | first is 1
numbres | nth 0 is 1

numbers | size         is 3
numbers | max          is 3
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

nemo = Pet { name: Nemo, age: 8 } <<--------------------------- how would it work with `8years`?
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

fun route(ip_kind: IpAddres)
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

IpVersion | V6 vs IpVersion::V6 <------------------------------ enum access? leaning towars second version, imports uses same sintax, who about both

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

# Variables
```
a = 1                <<---------------------------------------- in-mutable?
a is 1

a | inc is 2
a is 1

mut x := 1                 <<---------------------------------- mutable?
mut x = 1                  <<---------------------------------- other option
x := 1                  
x = 1

x := 2

x | inc!             <<--------------------------------------- bang methods for mutables
x is 26

for it in 1..3
  it | print
end
```

# Conditionals
```
if 42 > 22 and it < 50 is true <<------------------------------- "it" without pipe in boolean operation?
  "be happy"
else if not true
  "be happy"
else
  "be happy"
end

true ? verdad : falso is verdad

walk() if age > 1

true and true is true
true && true  is true                <<----------------------- and vs &&

41 | it > 30 and it < 50 is true

human = Human { Steven , 30 }
human | age >= 21 and it type? Human   is true 
human | age >= 21 and it is type Human is true 
human | age >= 21 and ( it | fly? )    is false 
human | age >= 21 and fly?             is false 
```

## Functions
```
# Fibonacci Sequence
fun fib(number: Int)
  number <= 1 ? number : number - 1 | fib
check
  0 | fib is 0
  1 | fib is 1
  3 | fib is 5
end

[1 2] | map fib is [1 3]

fn other()
  1 2
end

a b = other()

# product and addition
fun proAddition(a: Int b: Int): Int Int
  a * b, a + b <-------------------------------- how to multiple returns?
end
```

# Lambdas
```
it is "it" and type String

1 | it              is 1
1 | (it) { it + 1 } is 2
1 | it + 1          is 2

inc = it + 1 <----------------------------------- do not allow lamda assignation?!
1 | inc is 2

inc is type Int: Int <--------------------------- anotated lamdas
inc is type Lambda(Int): Int

10    | it * it is 20
10 20 | a + b   is 30

[1 2] | map    x + 1       is [2 3]
[1 2] | map    double      is [2 4]
[1 2] | reduce it + acc    is 3

[nemo dory] | map double raises Error { no-method double(:Pet) }
```

# Generic types
```
fun map(iterator: kind[] action(kind): kind) kind[] <-------- Anotated lambdas
  mut result = kind[] <---------------------------------------- mutable!
  for elem in iterator
    result | insert action(elem)
    result | insert elem | action <----------------------------- How to do double pipe?

    result | insert(elem | action)

    a = elem | action
    result | insert a
  end

  result
end
```

# Loops
```
for x in 0..42
  x | double | print
end

0..3 | map { x | double } is [0 2 4 6]

a = 0..3 | map double
a is [0 2 4 6]

0..3  | reduce x + sum is 6
0..42 | max is 42 
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
# FizzBuzz
fun fizzbuzz(x: Int)
  if x mod 15 eq 0
    FizzBuzz
  else if x mod 3 eq 0
    Fizz
  else if x mod 5 eq 0
    Buzz
  end
end

..100 | map fizzbuzz | print
```
