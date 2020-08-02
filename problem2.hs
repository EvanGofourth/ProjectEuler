-- The classic recursive definition of fib.
fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2) 

-- Fibonacci table reveals the 33rd fibonacci number
-- is the last not to exceed 4,000,000.
all_even_fibs = [ fib i | i <- [1..33] , (fib i) `mod` 2 == 0]

main = do
  print(sum all_even_fibs) 
