module LongSleep where

import Control.Concurrent (threadDelay)

longSleep :: Integer -> IO ()
longSleep micros
  | micros < 0 = error "negative sleep"
  | micros == 0 = return ()
  | micros < 1000000 = threadDelay (fromInteger micros)
  | otherwise = do
      threadDelay 1000000
      longSleep (micros - 1000000)  

