module Watchdog where

import Control.Concurrent
import Control.Exception

import Types
import LongSleep
import Engine
import GameState

type Watchdog = Prediction -> IO ()

newWatchdog :: MVar Game -> IO Watchdog
newWatchdog gmv = do
  dogmv <- forkIO hang >>= newMVar
  return $ \next -> do
    mth <- tryTakeMVar dogmv
    case mth of
      Nothing -> throwIO (userError "kickDog: watchdog thread not found")
      Just th -> killThread th
    th <- forkIO (watchdogThread gmv next)
    putMVar dogmv th
    
watchdogThread :: MVar Game -> Prediction -> IO a
watchdogThread gmv next = loop next where
  loop Never = hang
  loop (NotBefore dt) = do
    longSleep dt
    next <- modifyMVar gmv $ \g -> do
      let g' = elapse dt g
      let next = detect g'
      return (g', next)
    loop next
  loop (InExactly dt occ) = do
    longSleep dt
    next <- modifyMVar gmv $ \g -> do
      let g' = elapse dt g
      let (g'', output) = poke occ g'
      let next = detect g'
      --output
      return (g', next)
    loop next
