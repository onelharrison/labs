# Examples of Structural Pattern Matching in Ruby 2.7

def sum(arr)
  case arr
    in []; 0
    in x, *rest; x + sum(rest)
  end
end

def reverse(arr)
  case arr
    in []; []
    in x, *rest; reverse(rest) + [x]
  end
end

def factorial(n)
  # assumes n: Integer >= 1
  case n
    in 1; 1
    else; n * factorial(n - 1)
  end
end

# ----------------
puts 'sum...'
puts "sum([]): #{sum([])}"
puts "sum([1]): #{sum([1])}"
puts "sum([1, 2]): #{sum([1, 2])}"
puts "sum([1, 2, 3]): #{sum([1, 2, 3])}"
puts "sum([1, 2, 3, 4, 5]): #{sum([1, 2, 3, 4, 5])}"
 
puts
puts 'reverse...'
puts "reverse([]): #{reverse([])}"
puts "reverse([1]): #{reverse([1])}"
puts "reverse([1, 2]): #{reverse([1, 2])}"
puts "reverse([1, 2, 3]): #{reverse([1, 2, 3])}"
puts "reverse([1, 2, 3, 4, 5]): #{reverse([1, 2, 3, 4, 5])}"
 
puts
puts 'factorial...'
puts "factorial(1): #{factorial(1)}"
puts "factorial(2): #{factorial(2)}"
puts "factorial(3): #{factorial(3)}"
puts "factorial(4): #{factorial(4)}"
puts "factorial(5): #{factorial(5)}"
