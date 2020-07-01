{-# OPTIONS_GHC -Wall #-}

---- Exercise 1
---- I'm sure nth can be shorter, maybe using filter somehow

nth :: [a] -> Int -> Int -> [a]
nth [] _ _ = []
nth (x:xs) k n
  | n == k = x : (nth xs 1 n)
  | otherwise = nth xs (k + 1) n

skips :: [a] -> [[a]]
skips x = map (nth x 1) [1 .. (length x)]

-- Exercise 2
-- Local maxima

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | x < y && z < y = y:(localMaxima (z:xs))
  | True = localMaxima (y:z:xs)
localMaxima _ = []

{-
I started this one by dealing with the empty, single, and double element lists
first, like

localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (x:...) = ...

But then I realized that with pattern matching, I could deal with all these cases
at once just by shifting the main case to the top and using the wildcard to deal
with the remaining issues.
-}

-- Exercise 3
histogram :: [Integer] -> String



------------------ EOF
