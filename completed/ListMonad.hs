
{-* The List monad -} 

-- We can represent linked lists as follows:

data List a = Nil | Cons a (List a)
  deriving Show

{- An element of [List a] is either [Nil] or a [Cons]-cell on top of
another list (the tail). For example, the list [1, 2, 3] is
represented as: -}

my_list = Cons 1 (Cons 2 (Cons 3 Nil))

-- The sum function, for our hand-rolled lists:
sum' :: List Int -> Int
sum' Nil = 0
sum' (Cons x xs) = x + sum' xs

{-* Check: [sum' my_list] -}

{-* Our definition of [List] is *polymorphic*, meaning the entries of
our lists can have any type. This makes [List] into a *type
constructor*, a function into types.

 - Check: [:k List]

Roughly, a type constructor which also acts on functions is a *functor*.

 - Check: [:t fmap]. -}

instance Functor List where
  -- fmap :: (a -> b) -> (List a -> List b)
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

{-* Try: [fmap (+2) my_list]. -}

concat' :: List a -> List a -> List a
concat' Nil x = x
concat' (Cons x xs) ys = Cons x (concat' xs ys)

instance Applicative List where
  -- pure :: a -> List a
  pure x = Cons x Nil

  -- (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> vs = Nil
  (Cons f fs) <*> vs = concat' (fmap f vs) (fs <*> vs)

my_f_list = Cons (+ 2) (Cons (* 3) Nil)

{-* Try: [my_f_list <*> my_list]. -}

instance Monad List where
  -- (>>=) :: List a -> (a -> List b) -> List b
  Nil >>= f = Nil
  (Cons x xs) >>= f = concat' (f x) (xs >>= f)

{-* Monads come with "do"-notation which makes them pleasant to work
with. We'll see this in action for our parsers shortly! -}


