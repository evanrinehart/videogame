{-# LANGUAGE GADTs #-}
module Audio where

import SDL

import Data.Int
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as V
import Control.Monad 
import Control.Exception
import Control.Concurrent.STM

data Sound =
  CoinSound |
  RegistrateSound
    deriving Show

data SoundCtrl =
  PlaySound Sound
    deriving Show

audioCallback :: TChan SoundCtrl -> AudioFormat x -> IOVector x -> IO ()
audioCallback ctrlCh Signed16BitLEAudio outbuf = do
  msgs <- atomically (readAlot ctrlCh)
  when (not (null msgs)) $ do
    putStr "AUDIO: "
    print msgs
  let sampleCount = V.length outbuf
  forM_ [0..sampleCount-1] $ \i -> do
    V.write outbuf i 0

readAlot :: TChan a -> STM [a]
readAlot ch = go [] where
  go accum = do
    result <- tryReadTChan ch
    case result of
      Nothing -> return (reverse accum)
      Just x -> go (x : accum)
