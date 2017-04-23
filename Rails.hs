module Rails where

import Types

type Speed = Rational
newtype SampleRate = SampleRate Integer

sr = 44100

speed2counterMax :: Speed -> Integer
speed2counterMax 0 = 0
speed2counterMax s = max 1 (floor ((fromInteger sr) / s))

data Rail = Rail
  { railValue :: !Integer
  , railSize :: !Integer
  , railCounter :: !Integer
  , railSpeed :: Integer -> Speed
  }

instance Show Rail where
  show (Rail v m c s) = "Rail " ++ show v ++ "/" ++ show m ++ " " ++ show c ++ " (" ++ show (s v) ++ ")"

advanceRail :: Delta -> Rail -> Rail
advanceRail dt r@(Rail v m c s)
  | dt < 0 = error "advanceRail negative"
  | v == (m-1) = r
  | c == 0 = r
  | dt == 0 = r
  | dt < c = Rail v m (c - dt) s
  | otherwise = advanceRail (dt - c) (Rail (v+1) m c' s) where
      c' = speed2counterMax (s (v+1))

simpleRail :: Integer -> Speed -> Rail
simpleRail size speed = Rail 0 size c0 s where
  s _ = speed
  c0 = speed2counterMax (s 0)

unfoldRail :: Rail -> [(Delta, Integer)]
unfoldRail r = go r 0 where
  go r@(Rail v m c s) dt
    | c == 0 || (s v == 0) = [(dt, v)]
    | v < m - 1 = (dt, v) : go (advanceRail c r) (dt + c)
    | otherwise = [(dt, v)]

zipWithRails :: (Delta -> Integer -> Integer -> a) -> Rail -> Rail -> [a]
zipWithRails f r1 r2 = go (unfoldRail r1) (unfoldRail r2) (railValue r1) (railValue r2) where
  go arg1@((dt1, v1'):xs1) arg2@((dt2, v2'):xs2) v1 v2
    | dt1 < dt2 = f dt1 v1' v2 : go xs1 arg2 v1' v2
    | dt1 > dt2 = f dt2 v1 v2' : go arg1 xs2 v1 v2'
    | otherwise = f dt1 v1' v2 : f dt1 v1' v2' : go xs1 xs2 v1' v2'
  go [] arg2@((dt2, v2'):xs2) v1 v2 = f dt2 v1 v2' : go [] xs2 v1 v2'
  go arg1@((dt1, v1'):xs1) [] v1 v2 = f dt1 v1' v2 : go xs1 [] v1' v2
  go [] [] _ _ = []

