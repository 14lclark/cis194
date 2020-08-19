{-# OPTIONS_GHC -Wall #-}

-- Anonymous functions

greaterThan100 :: [Integer] -> [Integer]

gt100 :: Integer -> Bool
gt100 x = x > 100

greaterThan100 = filter gt100

--- Instead, use an anonymous (aka lambda) func. for gt100 b/c we'll never use it again

greaterThan100' :: [Integer] -> [Integer]
greaterThan100' = filter (\x -> x > 100)


-- Can also have multiple arguments

arrMake = \x y z -> [2*x,3*y,4*z]


--- Better way to write greaterThan100 is with an operator section
--- (which I have already been using)

greaterThan100'' :: [Integer] -> [Integer]
greaterThan100'' = filter (>100)


----- Function composition (which I have also already been using)

comp :: (b -> c) -> (a -> b) -> (a -> c)
comp f g = \x -> f (g x)

-- Useful in a wholemeal style programming

myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))
-- can be rewritten as
myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100


----- Currying and Uncurrying

f :: Int -> Int -> Int
f x y = 2*x + y

f' :: Int -> (Int -> Int)
f' x y = 2*x + y

-- These two functions are equivalent.
-- We can also uncurry this function so that it accepts a tuple instead.

f'' :: (Int,Int) -> Int
f'' (x,y) = 2*x + y

-- We can think of f'' as taking two arguments, but it's only actually taking one.

-- We can make two functions "curry" and "uncurry" to convert between these styles.

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f x y = f (x,y)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (x,y) = f x y

-- uncurry can be useful when you want to apply a function to a pair,
-- such as in @ uncurry (+) (2,3) @

----- Wholemeal Programming

foobar :: [Integer] -> Integer
foobar [] = 0
foobar (x:xs)
  | x > 3 = (7 * x + 2) + foobar xs
  | otherwise = foobar xs

-- This is not good Haskell style. It is
--  - doing too much at one; and
--  - working at too low of a level

-- A more idiomatic implementation:

foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7*x + 2) . filter (>3)


----- Folds

sum' :: [Integer] -> Integer
sum' []     = 0
sum' (x:xs) = x + sum' xs

product' :: [Integer] -> Integer
product' []     = 1
product' (x:xs) = x * (product' xs)

length' :: [Integer] -> Integer
length' []     = 0
length' (_:xs) = 1 + length' xs

-- These all have a very similar structure which is encompassed by fold.

fold :: b -> (a -> b -> b) -> [a] -> b
fold z f [] = z
fold z f (x:xs) = f x (fold z f xs)

-- We can rewrite sum, product and length as follows:

sum'' = fold 0 (+)
product'' = fold 1 (*)
length'' = fold 0 (\_ s -> 1 + s)

length'''  = fold 0 (\_ -> (1+))
length'''' = fold 0 (const (1+))






----------------------------- EOF
