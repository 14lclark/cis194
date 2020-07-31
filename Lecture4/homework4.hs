{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-------- Exercise 1

---- 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * (fun1 xs)
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . (map (+(-2))) . (filter even)

---- 2

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- The hint is to use iterate and takeWhile

fun2' :: Integer -> Integer
fun2' =
  sum .
  filter even .
  takeWhile (\n -> n /= 1) .
  iterate
    (\n ->
       if even n
         then n `div` 2
         else 3 * n + 1)

---- This one took me a lot longer than I'd like to admit.

----   10 -> 10 + fun2(3 * 5 + 1) -> 10 + 16 + fun2(16 / 2) -> ... ->  10 + 16 + 8 + 4 + 2 + 0
----   7  -> fun2(3 * 7 + 1) -> 22 + fun2(11) -> 22 + 34 + fun2(34/2) -> 22 + 34 + 52 + 26 + fun2(13)


----- Exercise 2

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Integral b => [a] -> b
height x = floor $ (log $ fromIntegral $ length x) / (log 2)


foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree xs = Node (height xs)
                (foldTree $ take half xs)
              middle
                (foldTree $ drop (half+1) xs)
              where
                half = (length xs) `div` 2
                middle = xs !! half

--- I had to look up a solution for this one after a while to finish it.


----- Exercise 3

--- 1

xor :: [Bool] -> Bool
xor = (== 1) . (`mod` 2) . foldr (+) 0 . map trueIsOne

trueIsOne :: Bool -> Integer
trueIsOne x
  | x = 1
  | otherwise = 0

-- I did this originally with lambdas and if-then-else, but decided to just write out the functions instead

--- 2 -- Implement map using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []


--- 3 (optional) -- Implement foldl using foldr
--myFoldl :: (a -> b -> a) -> a -> [b] -> a
--myFoldl f base xs = foldr (\x y -> f y x) base (reverse xs)

----- Exercise 4 -- Sieve of Sundaram
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

notElem' :: (Eq a) => [a] -> a -> Bool
notElem' x y = not (elem y x)

removals :: Integer -> [Integer]
removals n = [i + j + 2*i*j | (i,j) <- (cartProd [1..n] [1..n])]



sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  map (\x -> 2 * x + 1) $ filter (notElem' (removals n)) [3 .. n]





-------------------------- EOF
