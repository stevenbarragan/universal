

# functions
# variables definition
# variables assignation

def factorial(n: i32) : i32
  if n == 1
    1
  else
    n * factorial(n - 1)
end

factorial(n: i32) : i32
factorial(1): 1 if n == 1
factorial(n): n * factorial(n - 1)

fn add2(x: i32, y: i32) : i32
  x + y
end
