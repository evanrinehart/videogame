module Splash where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad
import Data.Monoid

import Control.Monad.Random
import Linear

import Debug
import Types
--import HighScore
import Graphics
--import Animation
import Audio
import System
--import Credits
import Util
import {-# SOURCE #-} TitleScene

splashScene :: System -> Fix IO
splashScene sys@(Sys vout ctrlIn soundCtrl _ _ _) = Fix $ do
  atomically (writeTChan soundCtrl (PlaySound SplashSound))
  withAsync (randomBits vout) $ \_ -> do
    threadDelay 400000
    return (titleScene sys)

randomBits :: VideoOut -> IO a
randomBits vout = forever $ do
  bits <- evalRandIO (replicateM (40*30) getRandom)
  let ixs = zip bits [(i*8,j*8) | i <- [0..39], j <- [0..29]]
  let f (False,(i,j)) = Shift (V2 i j) (Sprite (V4 0 0 8 8))
      f (True, (i,j)) = Shift (V2 i j) (Sprite (V4 8 0 8 8))
  let items = map f ixs
  let pic = mconcat items
  writeVideo vout pic
  writeVideo vout pic
  writeVideo vout pic

