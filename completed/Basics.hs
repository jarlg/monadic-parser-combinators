
{-* An introduction to (parts of) Haskell -}

{- Below are some familiar definitions and functions in the (possibly)
unfamiliar language of Haskell. You can paste these into 'ghci' and
experiment to gain familiarity. -}

{- A function definition in Haskell consists of a(n optional) *type
signature* and a *function body*. Below is a function which takes an
integer and adds two to it: -}

plustwo :: Int -> Int
plustwo n = n + 2

{-* Try:
  1. load this file into GHCi using [:l Basics]
  2. evaluate the function 'plustwo' on some input
-}

-- Type signatures are optional because GHC can (usually) guess them.

{-* Try:
  1. comment out the type signature of 'plustwo' above
  2. reload this file using [:r] in GHCi
  3. check the inferred type signature using [:t plustwo]
-}


{-** Recursive and inductive function definitions -}

{- Haskell functions are "pure", meaning that they can only "do" what
their type signature allows. In particular, a function can only call
other functions in producing its output. This makes recursion very
natural, and iteration (as in for- and while-loops) less so. -}

-- Example: a recursive definition of the factorial
fact :: Int -> Int
fact 0 = 1
fact n = n * (fact (n-1))

-- Alternatively we can write out the if-branch:
fact' n = if n == 0 then 1 else n * (fact (n-1))


{-* Exercise: write a "modified" factorial function which only
multiplies the odd factors of an integer.
  (Hint: use the predicate [odd :: Int -> Bool].) -}

oddfact :: Int -> Int
oddfact 0 = 1
oddfact n = if odd n then n * (oddfact (n-1)) else (oddfact (n-1))

-- a faster version:
oddfact' 0 = 1
oddfact' 1 = 1
oddfact' n = if odd n then f n else f (n-1)
  where f m = m * f (m-2)
  

{-** Lists and strings -}

{- We will be particularly interested in strings, which will be linked
lists of characters for us. -}

my_int_list = [1,2,3] :: [Int]
my_string = "Hello, world!" :: String  -- or [Char]

{-* These linked lists are built up from the empty list '[]' by
sequentially attaching cells. The ""-notation is syntactic sugar for
the cons-operator which constructs lists.

Exercise:
 1. Check the type of the cons operator [:t (:)]
 2. Check: ["abc" == ('a' : 'b' : 'c' : [])]
-}

{- A cool aspect of Haskell is that it is "lazily" evaluated; this
lets us represent infinite lists and unbounded recursion. This is
computationally fine as long as we only use a finite number of
elements of these lists. -}

inf :: [Int]
inf = inf' 0
  where inf' n = n : inf' (n+1)

{-* Try: [take 10 inf] -}

-- There is already a shorthand:
inf' = [0, 1..]

{- We can define functions on lists by *pattern matching* on the empty
list and a cons-cell (a non-empty list), as in the following examples.

This function computes the sum of a list of integers. -}

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + (sum xs)

{- This function filters out elements from a list which don't satisfy
the given predicate. -}
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' pred (x:xs) = if pred x then (x : filter pred xs) else (filter pred xs)


{- You may be familiar with the function 'map', which takes a
function and applies it to each element of a list. In GHCi, write
[import Data.Char (toUpper)] and try: [map toUpper my_string]. -}

{-* Exercise: write your own version called [map']. -}

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x) : (map f xs)


{-* Exercise: Define the function [count] which counts the number of
occurrences of a character in a string. -}

count :: Char -> String -> Int
count _ [] = 0
count c (x:xs) = if x == c then 1 + count c xs else count c xs


{-** List comprehension -}

{- An elegant way to define new lists from old lists is using list
comprehensions. This is a convenient syntax for creating and filtering lists. For example, here are two definitions of the list of all even numbers: -}

evens = [ 2 * x | x <- [0, 1..] ]

evens' = [ x | x <- [0, 1..], even x ]

{-* Exercise: Write the functions [map] and [filer] list comprehensions. -}

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = [ f x | x <- xs ]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' pred xs = [ x | x <- xs, pred x ]
