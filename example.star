fn factorial(n: i32) : i32
  if n == 1
    1
  else
    factorial(n - 1) * n
  end
end

factorial(20)
