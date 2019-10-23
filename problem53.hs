factorial :: Integer->Integer
factorial n = product [1..n]

combine :: Integer->Integer->Integer
combine n r = (factorial n) `div` ((factorial r) * (factorial (n-r)))

count_GT_one_mil :: [Integer]->Integer->Integer->Integer
count_GT_one_mil [] _ _ = 0
count_GT_one_mil (x:xs) r it
  | r > x = it + count_GT_one_mil xs 1 it
  | (combine x r) > 1000000 = 1 + count_GT_one_mil (x:xs) (r+1) it
  | otherwise = count_GT_one_mil (x:xs) (r+1) it

main = do
  print(count_GT_one_mil [1..100] 1 0)
