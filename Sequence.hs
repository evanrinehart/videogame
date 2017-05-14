{-# LANGUAGE DeriveFunctor #-}
module Sequence where

import Delta

data Sequence a = Ended | Incoming Delta a (Sequence a)
  deriving (Functor, Show)

step :: Sequence a -> Either (Sequence a) (a, Sequence a)
step Ended = Left Ended
step (Incoming 0 x next) = Right (x, next)
step (Incoming timeLeft x next) = Left (Incoming (timeLeft - 1) x next)

fromList :: [(Delta,a)] -> Sequence a
fromList xs = go 0 xs where
  go t ((t',x):rest) = Incoming (t'-t) x (go t' rest)
  go _ [] = Ended

instance Monoid a => Monoid (Sequence a) where
  mempty = Ended
  mappend (Incoming dt1 x next1) (Incoming dt2 y next2)
    | dt1 < dt2 = Incoming dt1 x (mappend next1 (Incoming (dt2 - dt1) y next2))
    | dt2 < dt1 = Incoming dt2 y (mappend (Incoming (dt1 - dt2) x next1) next2)
    | otherwise = Incoming dt1 (mappend x y) (mappend next1 next2)
  mappend Ended ys = ys
  mappend xs Ended = xs

periodic :: Delta -> a -> Sequence a
periodic period x = Incoming period x (periodic period x)

