{-# LANGUAGE GADTs #-}
module Animation where

import Types

data Animation a where
  Pure :: a -> Animation a
  Fmap :: (a -> b) -> Animation a -> Animation b
  Appl :: Animation (a -> b) -> Animation a -> Animation b
  Primitive :: a -> (Delta -> a -> a) -> Animation a
  After :: Delta -> Animation a -> (Animation a -> Animation a) -> Animation a
  Split :: Animation a -> Animation a -> Animation a
  Expired :: a -> Animation a
  Fire :: Output -> Animation a -> Animation a
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
  Fire out a -> view a
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
  Fire out a -> a0
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
    Fire out a -> m
    Parallel as -> minimum (map (go m) as)
  
reduce :: Animation a -> (Animation a, [Output])
reduce a0 = go a0 [] where
  go :: Animation a -> [Output] -> (Animation a, [Output])
  go a0 outs = case a0 of
    Pure x -> (a0, outs)
    Fmap f a -> let (a',outs') = go a outs in (Fmap f a', outs')
    Appl af ax ->
      let (af', outs') = go af outs in
      let (ax', outs'') = go ax outs' in
      (Appl af' ax', outs'')
    Primitive x f -> (a0, outs)
    After counter a k ->
      let (a', outs') = go a outs in (After counter a' k, outs)
    Split a1 a2 -> (a0, outs)
    Expired x -> (a0, outs)
    Fire out a -> let (a', outs') = go a outs in (a', out:outs')
    Parallel as ->
      let (as', outs') = goPar as outs [] in
      (Parallel (reverse as'), outs')
  goPar :: [Animation a] -> [Output] -> [Animation a] -> ([Animation a], [Output])
  goPar [] outs accum = (accum, outs)
  goPar (a:as) outs accum =
    let (accum',outs') = go1 a outs accum in
    goPar as outs' accum'
  go1 :: Animation a -> [Output] -> [Animation a] -> ([Animation a], [Output])
  go1 a0 outs accum = case a0 of
    Expired _ -> (accum, outs)
    Fire out a -> go1 a (out:outs) accum
    Split a1 a2 ->
      let (accum', outs') = go1 a1 outs accum in
      go1 a2 outs' accum'
    other -> let (a', outs') = go a0 outs in (a':accum, outs')
