{-# LANGUAGE DeriveFunctor #-}
module Varying where

import Delta
import Vectors

data Varying a =
  Etern a |
  Ephem a Delta (Varying a)
    deriving (Show, Functor)

fromList :: a -> [(Delta,a)] -> Varying a
fromList x [] = Etern x
fromList x0 ((d,x1):xs) = Ephem x0 d (fromList x1 xs)

at :: Delta -> Varying a -> a
at dt (Etern x) = x
at dt (Ephem x dt1 next)
  | dt < dt1 = x
  | otherwise = at (dt - dt1) next

step :: Varying a -> Varying a
step v@(Etern x) = v
step (Ephem x 0 next) = next
step (Ephem x timeLeft next) = Ephem x (timeLeft - 1) next

current :: Varying a -> a
current (Etern x) = x
current (Ephem x _ _) = x

timeZip :: (a -> b -> c) -> Varying a -> Varying b -> Varying c
timeZip f (Etern x) (Etern y) = Etern (f x y)
timeZip f v1@(Etern x) (Ephem y dt next) = Ephem (f x y) dt (timeZip f v1 next)
timeZip f (Ephem x dt next) v2@(Etern y) = Ephem (f x y) dt (timeZip f next v2)
timeZip f (Ephem x dt1 next1) (Ephem y dt2 next2)
  | dt1 < dt2 = Ephem (f x y) dt1 (timeZip f next1 (Ephem y (dt2 - dt1) next2))
  | dt1 > dt2 = Ephem (f x y) dt2 (timeZip f (Ephem x (dt1 - dt2) next1) next2)
  | otherwise = Ephem (f x y) dt1 (timeZip f next1 next2)

instance Monoid a => Monoid (Varying a) where
  mempty = Etern mempty
  mappend = timeZip mappend

instance Applicative Varying where
  pure x = Etern x
  vf <*> vx = timeZip ($) vf vx

uniform :: a -> (a -> a) -> Speed -> Varying a
uniform x0 f speed = go x0 where
  go x = Ephem x dt (go (f x))
  dt = perSecond (abs speed)

dynamics :: Integer -> Varying Speed -> Varying Integer
dynamics x0 (Etern v)
  | v == 0 = Etern x0
  | otherwise = uniform x0 (+ floor (signum v)) v
dynamics x0 (Ephem 0 timeLeft next) = Ephem x0 timeLeft (dynamics x0 next)
dynamics x0 (Ephem v timeLeft next) = Ephem x0 dt next' where
  dt = perSecond (abs v)
  next' | dt < timeLeft = dynamics (x0 + floor (signum v)) (Ephem v (timeLeft - dt) next)
        | dt >= timeLeft = dynamics (x0 + floor (signum v)) next

bound :: Ord a => a -> a -> Varying a -> Varying a
bound lower upper (Etern x)
  | x <= lower = Etern lower
  | x >= upper = Etern upper
  | otherwise = Etern x
bound lower upper (Ephem x dt next)
  | x <= lower = Etern lower
  | x >= upper = Etern upper
  | otherwise = Ephem x dt (bound lower upper next)

{-
type Location = V2 Integer
type Motion = Varying Location

motion2D :: Varying Integer -> Varying Integer -> Motion
motion2D x y = V2 <$> x <*> y

derivative :: Varying Integer -> Speed
derivative (Etern _) = 0
derivative (Ephem x dt next) =
  let x' = current next in
  (fromInteger (x' - x) / fromInteger dt)
-}
