module Chrono where

import Data.IORef
import System.Clock

data Chrono = Chrono
  { chronoDiff    :: Integer -> IO Integer
  , chronoSetTime :: Integer -> IO ()
  , chronoGetCurrent :: IO Integer }

newChrono :: IO Chrono
newChrono = go where
  chronoDiff ref nanos1 = do
    nanos0 <- readIORef ref
    return (nanos1 - nanos0)
  chronoSetTime ref t = do
    writeIORef ref t
  chronoCurrent = fmap toNanoSecs (getTime Monotonic)
  go = do
    now <- fmap toNanoSecs (getTime Monotonic)
    ref <- newIORef now
    return $ Chrono
      (chronoDiff ref)
      (chronoSetTime ref)
      chronoCurrent
