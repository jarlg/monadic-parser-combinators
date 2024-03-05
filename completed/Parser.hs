import Data.Char (isDigit, ord, isSpace)

{-* Monadic Parser Combinators

We begin by defining our generic parser type. A parser is a function
which takes in a string and consumes part of this string in order to
produce some (or several possible) "return values". The parser then
outputs those values, along with the remaining (unconsumed) strings.

Returning a list of possible results lets us explore different parse
trees. If the returned list is empty, then the parser failed.

The 'data' keyword lets us define this type: -}

data Parser a = Parser (String -> [(a, String)])

{- The following function lets us run a parser: -}

parse :: Parser a -> String -> [(a, String)] 
parse (Parser p) s = p s

{- As an example, here is a parser which only accepts a specified 
character [c]. -}

char :: Char -> Parser Char
char c = Parser p
  where p [] = []
        p (x:xs) = if x == c then [(c, xs)] else []

{-* Try: [parse (char 'a') "abc"] and [parse (char 'b') "abc"]. -}

{- Another example, here is a parser which reads a single
digit. (Later on we will reconstruct this parser from more primitive
ones.) -}

oneDigit :: Parser Int
oneDigit = Parser p
  where p [] = []
        p (x:xs) = if isDigit x then [(ord x - ord '0', xs)] else []

{-* Try: [parse oneDigit "123"] -}

{-* Exercise:
  1. define the parser [failure] which always fails (returns an empty list),
  2. define the parser [item] which parses a single character (of any
type), and returns that character. -}

failure :: Parser a
failure = Parser (\s -> [])

item :: Parser Char
item = Parser p
  where p [] = []
        p (x:xs) = [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat pred = Parser p
  where p [] = []
        p (x:xs) = if pred x then [(x, xs)] else []

{-* Exercise: redefine [char] above in terms of [sat]. -}

char' :: Char -> Parser Char
char' c = sat pred
  where pred x = (c == x)

-- Alternatively:
char'' c = sat (== c)


{-* We now make [Parser] into a monad. -}

{- The functoriality of our parsers lets us apply functions to parse results. -}
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser (\s -> [ (f x, xs) | (x, xs) <- parse p s ])


{- If we have a parser of functions, we can apply these functions to
other parse results. -}
instance Applicative Parser where
  -- pure :: a -> Parser a
  -- This "parser" simply returns a value without touching the input.
  pure x = Parser (\s -> [(x, s)])

  fs <*> vs = Parser p
    where p s = [ (f v, xs')
                | (f, xs) <- parse fs s,
                  (v, xs') <- parse vs xs ]


{- If we have a parser (of some values), and a function which creates
parsers out of those values, then we can combine these as follows: -}
instance Monad Parser where
  vs >>= fp = Parser p
    where p s = concat [ parse (fp x) xs | (x, xs) <- parse vs s ]


{- We can also run multiple parsers "in parallel", or sequentially
(the other runs only if the former fails). -}

(<+>) :: Parser a -> Parser a -> Parser a
p <+> q = Parser (\s -> parse p s ++ parse q s)

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser (\s -> case parse (p <+> q) s of
                     [] -> []
                     (x:xs) -> [x])

string :: String -> Parser String
string "" = return ""
string (x:xs) = do
  c <- char x
  cs <- string xs
  return (c:cs)

{-* Try: [parse (string "Hello") "Hello world!"]. -}
  
{- A combinator which runs a given parser one or more times. -}
many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

{- A parser which consumes zero or more whitespaces. -}
space :: Parser ()
space = do
  _ <- many (sat isSpace)
  return ()

until' :: Char -> Parser Char
until' c = do
  x <- item
  if x == c
    then return c
    else until' c

{- The following parser takes a string of the form 'name: <name>' and
  parses just the name. -}
example :: Parser String
example = until' ':' >> space >> many item

{-* Try: [parse example "name: <your name>"]. -}
  

{- As a another example, here is a parser for arithmetic expressions
with two terms and one operator. -}

addop = char '+' >> return (+)
mulop = char '*' >> return (*)

expr :: Parser Int
expr = do
  space
  n <- oneDigit
  space
  op <- addop <|> mulop
  space
  m <- oneDigit
  return (op n m)

{-* Try: [parse expr "5 * 7"].  -}


{-* Now try to work through sections 6 - 8 to define a parser for
basic arithmetic expressions! -}

  
