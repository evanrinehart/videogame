module Watchdog where

import Control.Concurrent
import Control.Exception

import Types
import LongSleep
import Engine
import Game

kickWatchdog :: Prediction -> MVar Game -> MVar ThreadId -> IO ()
kickWatchdog next gmv dogmv = go where
  go = do
    mth <- tryTakeMVar dogmv
    case mth of
      Nothing -> throwIO (userError "kickWatchdog: watchdog thread not found")
      Just th -> killThread th
    th <- forkIO (loop next)
    putMVar dogmv th
  loop Never = hang
  loop (NotBefore dt) = do
    longSleep dt
    next <- modifyMVar gmv $ \g -> do
      let g' = passTime dt g
      let next = detectNext g'
      return (g', next)
    loop next
  loop (InExactly dt occ) = do
    longSleep dt
    next <- modifyMVar gmv $ \g -> do
      let g' = passTime dt g
      let (g'', output) = poke occ g'
      let next = detectNext g'
      output
      return (g', next)
    loop next
