--------------------------------------------------------------------------------
--A Lychrel number finder --
--------------------------------------------------------------------------------
list_of_Lychrel_numbers_LT_10000 = ls
  where
    list = [1..10000]
    ls = (filter(/=(-1))) [if is_lychrel_number x then x else -1| x <- list]
    
--Is n a Lychrel number?
is_lychrel_number n = not (can_be_pal 50 (new_n n))

--Is n a Palindrome?
is_palindrome n
  | n_as_char_array == n_as_char_array_rev = True
  | n_as_char_array /= n_as_char_array_rev = False
    where
      n_as_char_array = show n
      n_as_char_array_rev = reverse(show n)

--Create a new n that is the sum of itself and its reverse.
new_n n = summ
  where
    n_as_char_array = show n
    n_rev = reverse n_as_char_array
    n_rev_value = read n_rev
    summ = n + n_rev_value

--Attempts to create a palindrome by iterating up to 50 times.
can_be_pal 1 n
  | is_palindrome n = True
  | not (is_palindrome n) = False

can_be_pal x n
  | is_palindrome n = True
  | not (is_palindrome n) = can_be_pal (x-1) (new_n n)


main = do
  print(length list_of_Lychrel_numbers_LT_10000)
