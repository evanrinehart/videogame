{-# LANGUAGE GADTs #-}
module Audio where

import SDL

import Data.Int
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as V
import Control.Monad 
import Control.Exception

audioCallback :: AudioFormat x -> IOVector x -> IO ()
audioCallback Signed16BitLEAudio outbuf = do
  let sampleCount = V.length outbuf
  forM_ [0..sampleCount-1] $ \i -> do
    V.write outbuf i 0
