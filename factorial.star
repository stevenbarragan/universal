export fn factorial(number: Int) : Int
  if number <= 1
    1
  else
    number * factorial(number - 1)
  end
end
