module Delta where

import Data.IORef
import System.Clock

-- returns an IO action which, when executed
-- returns the number of audio samples that have elapsed
-- since the last time it was called.
newDeltaGenerator :: Integer -> IO (IO Integer)
newDeltaGenerator sampleRate = do
  let unitsPerSecond = lcm sampleRate (10^9)
  let unitsPerNano = unitsPerSecond `div` (10^9)
  let unitsPerSample = unitsPerSecond `div` sampleRate
  now <- fmap toNanoSecs (getTime Monotonic)
  last <- newIORef now
  accum <- newIORef 0 -- accumulator is in 'units'
  return $ do
    nanos1 <- fmap toNanoSecs (getTime Monotonic)
    nanos0 <- readIORef last
    let nanoDelta = nanos1 - nanos0
    r <- readIORef accum
    let (samples, r') = (r + nanoDelta * unitsPerNano) `divMod` unitsPerSample
    writeIORef accum r'
    writeIORef last nanos1
    return samples
