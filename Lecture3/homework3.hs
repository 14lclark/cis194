{-# OPTIONS_GHC -Wall #-}

---- Exercise 1

nth :: [a] -> Int -> Int -> [a]
nth [] _ _ = []
nth (x:xs) k n
  | n == k = x : (nth xs 1 n)
  | otherwise = nth xs (k + 1) n

skips :: [a] -> [[a]]
skips x = map (nth x 1) [1 .. (length x)]
