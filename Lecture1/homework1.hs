{-# OPTIONS_GHC -Wall #-}

toDigits    :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
revList     :: [a]     -> [a]

revList = go []
          where
            go acc [] = acc
            go acc (x:xs) = go (x:acc) xs      

toDigitsRev x
  | x < 10 = [x]
  | otherwise = digit : (toDigitsRev next)
  where
    digit = x `mod` 10
    next = (x - digit) `div` 10

toDigits = revList . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther x = zipWith (*) alt x
  where
    alt' = cycle [1,2]
    alt = revList $ take (length x) alt'

sumDigits :: [Integer] -> Integer

sumDigits [] = 0
sumDigits (x:xs)
  | x < 10 = x + sumDigits xs
  | otherwise = first + second + sumDigits xs
  where
    first = x `mod` 10
    second = (x - first) `div` 10

validate :: Integer -> Bool

validate x
  | check x == 0 = True
  | otherwise = False
  where
    mod10 :: Integer -> Integer
    mod10 a = a `mod` 10
    check = mod10 . sumDigits . doubleEveryOther . toDigits

------------- Tower of Hanoi -------------

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi n a b c
  | n == 0 = []
  | n == 1 = j
  | otherwise = (go a c b) ++ j ++ (go c b a)
  where
    j = [(a, b)]
    go = hanoi (n - 1)
         
