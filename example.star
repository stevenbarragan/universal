def factorial(n: i32) : i32
  if n == 1
    1
  else
    n * factorial(n - 1)
end

factorial(20)

x = 25
y = 53 + x
c = y + factorial(2)

5 + 2

def greeting(name: String) : String
  "Hello #{ name }"
end

def yourname : Symbol
  let x: Symbol = :"steven barragan"

  x
end
