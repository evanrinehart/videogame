module Null where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad
--import Data.Monoid

import Debug
import Types
--import HighScore
import Graphics
--import Animation
import Audio
import System
--import Credits
import Util

blanker :: VideoOut -> IO a
blanker out = forever $ writeVideo out Blank

devNull :: ControlIn -> IO a
devNull inCh = forever $ do
  i <- readChan inCh
  logMsg "scene:null" (show i)

nullScene :: System -> Fix IO
nullScene sys =
  let vout = sysVideoOut sys in
  let inCh = sysCtrlIn sys in
  Fix $
    withAsync (blanker vout) $ \_ ->
    withAsync (devNull inCh) $ \_ -> do
      hang
