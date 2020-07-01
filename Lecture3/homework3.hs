{-# OPTIONS_GHC -Wall #-}

------- CODE GOLF HW
------- Write functions as short as possible,
------- not including types or spaces

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
localMaxima k =
  case k of
    (x:y:z:xs)
      | x < y && z < y -> y : (localMaxima (z : xs))
      | True -> localMaxima (y : z : xs)
    _ -> []

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

type Counter = Int
type Max     = Int

make :: Counter -> Max -> String
make n k = (go n "*\n") ++ (go (maximum [1, (k - n)]) " \n")
  where
    go a = (take $ 2 * a) . cycle

countOne :: [Counter] -> Int -> [Counter]
countOne acc x = map inc indAcc
  where
    indAcc = zipWith ((,)) [1 ..] acc
    inc (i, b)
      | x == i - 1 = b + 1
      | otherwise = b

count :: [Counter] -> [Int] -> [Counter]
count acc []     = acc
count acc (x:xs) = count (countOne acc x) xs

makeBarsList :: Max -> [Counter] -> String
makeBarsList m (y:ys) = unlines $ reverse $ go (y : ys) m
  where
    go (x:xs) k = zipWith (++) (lines $ make x k) (go xs k)
    go _ _ = cycle [""]
makeBarsList _ _ = ""

histogram :: [Int] -> String
histogram xs =
  ((makeBarsList $ maximum a) $ a) ++ ("==========\n") ++ ("0123456789")
          where
            a = count (take 10 $ cycle [0]) xs



------------------ EOF
