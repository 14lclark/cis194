data IntList
  = Empty
  | Cons Int IntList
  deriving (Show)

-- Map
mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

-- Filter
keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
  | even x = Cons x (keepOnlyEven xs)
  | otherwise = keepOnlyEven xs

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList f (Cons x xs)
              | f x = Cons x (filterIntList f xs)
              | otherwise = filterIntList f xs

-- Fold

foldrIntList :: (Int -> a -> a) -> a -> IntList -> a
foldrIntList f b Empty = b
foldrIntList f b (Cons x xs) = x `f` (foldrIntList f b xs)

foldlIntList :: (a -> Int -> a) -> a -> IntList -> a
foldlIntList f b (Cons c Empty) = b `f` c
foldlIntList f b (Cons x xs) = foldlIntList f (b `f` x) xs


-- Polymorphize our IntList into general List
data List t
  = E
  | C t (List t)
  deriving Show

-- Now we can rewrite the above functions in the general list setting

mapList :: (a -> b) -> List a -> List b
mapList _ E = E
mapList f (C x xs) = C (f x) (mapList f xs)


filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C x xs)
  | p x       = C x (filterList p xs)
  | otherwise = filterList p xs















---------- Total and Partial Functions

-- A partial function is a function for which there are certain values
-- which will cause the program to crash or recurse infinitely.
-- A total function is a function which has no such values.

-- head, tail, init, last, and (!!) are partial Prelude functions
-- which should be avoided.










-----------
