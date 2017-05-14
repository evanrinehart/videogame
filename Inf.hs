module Inf where

data AddInf a = Inf | Fin a deriving (Show, Eq)

instance Ord a => Ord (AddInf a) where
  compare Inf Inf = EQ
  compare (Fin x) (Fin y) = compare x y
  compare Inf _ = GT
  compare _ Inf = LT
