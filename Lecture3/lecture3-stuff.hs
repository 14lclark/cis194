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









----------
