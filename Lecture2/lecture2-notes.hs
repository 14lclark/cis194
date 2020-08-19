{-# OPTIONS_GHC -Wall #-}

data FailableDouble
  = Failure
  | OK Double
  deriving (Show)

safeDiv :: Double -> Double -> FailableDouble

safeDiv _ 0 = Failure
safeDiv a b = OK (a / b)

failureToZero :: FailableDouble -> Double              
failureToZero Failure = 0
failureToZero (OK d) = d

data Thing
  = Shoe
  | Ship
  | SealingWax
  | Cabbage
  | King
  deriving (Show)
                       
data Person =
  Person String Int Thing
  deriving (Show)

logan :: Person
logan = Person "Logan" 24 Shoe

getAge :: Person -> Int
getAge (Person _ a _) = a



