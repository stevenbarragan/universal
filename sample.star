import file/path

export fn hello: Int
  42
end

export fn factorial(number: Int) : Int
  edge: Int = 1

  if number <= edge
    1
  else
    number * factorial(number - 1)
  end
end

export fn floating: Float
  world = :world // symbol

  print "Hello #{ world }"

  42.42

  if true
    false
  else
    true
  end

  unless false
    true
  end
end
