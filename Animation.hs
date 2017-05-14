{-# LANGUAGE GADTs #-}
module Animation where

import Types
import Inf

data Animation a where
  Pure :: a -> Animation a
  Fmap :: (a -> b) -> Animation a -> Animation b
  Appl :: Animation (a -> b) -> Animation a -> Animation b
  Primitive :: a -> (Delta -> a -> a) -> Animation a
  After :: Delta -> Animation a -> (Animation a -> Animation a) -> Animation a
  Split :: Animation a -> Animation a -> Animation a
  Expired :: a -> Animation a
  Parallel :: [Animation a] -> Animation [a]

primitive :: a -> (Delta -> a -> a) -> Animation a
primitive = Primitive

after :: Delta -> Animation a -> (Animation a -> Animation a) -> Animation a
after = After

split :: Animation a -> Animation a -> Animation a
split = Split

expired :: a -> Animation a
expired = Expired

parallel :: [Animation a] -> Animation [a]
parallel = Parallel

isExpired :: Animation a -> Bool
isExpired (Expired _) = True
isExpired _           = False

instance Show a => Show (Animation a) where
  show a = show (view a)

instance Functor Animation where
  fmap = Fmap

instance Applicative Animation where
  pure = Pure
  (<*>) = Appl

instance Monoid a => Monoid (Animation a) where
  mempty = pure mempty
  mappend a1 a2 = mappend <$> a1 <*> a2
  
view :: Animation a -> a
view a0 = case a0 of
  Pure x -> x
  Fmap f a -> f (view a)
  Appl af ax -> (view af) (view ax)
  Primitive x f -> x
  After counter a k -> view a
  Split a1 a2 -> view a1
  Expired x -> x
  Parallel as -> map view as

elapse :: Delta -> Animation a -> Animation a
elapse dt a0 = case a0 of
  Pure x -> a0
  Fmap f a -> Fmap f (elapse dt a)
  Appl af ax -> Appl (elapse dt af) (elapse dt ax)
  Primitive x f -> Primitive (f dt x) f
  After counter a k
    | dt < counter  -> After (counter - dt) (elapse dt a) k
    | dt >= counter -> elapse (dt - counter) (k (elapse counter a))
  Split a1 a2 -> a0
  Expired x -> a0
  Parallel as -> Parallel (map (elapse dt) as)

next :: Animation a -> AddInf Delta
next a0 = go Inf a0 where
  go :: AddInf Delta -> Animation a -> AddInf Delta
  go m a0 = case a0 of
    Pure x -> m
    Fmap f a -> go m a
    Appl af ax -> min (go m af) (go m ax)
    Primitive x f -> m
    After counter a k -> min m (Fin counter)
    Split a1 a2 -> m
    Expired x -> m
    Parallel as -> minimum (map (go m) as)
  
reduce :: Animation a -> Animation a
reduce a0 = go a0 where
  go :: Animation a -> Animation a
  go a0 = case a0 of
    Pure x -> a0
    Fmap f a -> Fmap f (go a)
    Appl af ax -> Appl (go af) (go ax)
    Primitive x f -> a0
    After counter a k -> After counter (go a) k
    Split a1 a2 -> a0
    Expired x -> a0
    Parallel as -> Parallel (goPar as)
  goPar :: [Animation a] -> [Animation a]
  goPar (a:as) = case a of
    Expired _ -> goPar as
    Split a1 a2 ->
      let a1' = go a1 in
      let a2' = go a2 in
      a1' : a2' : goPar as
    other -> go a : goPar as
  goPar [] = []
