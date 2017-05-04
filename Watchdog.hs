{-# LANGUAGE BangPatterns #-}
module Watchdog where

import Control.Concurrent
import Control.Exception
import Control.Monad (forM_)
import System.Exit

import Types
import Common
import LongSleep
import Engine
import GameState
import Chrono
import Scene
import Output
import Error

type Watchdog = Prediction ([Output],Scene) -> IO ()

newWatchdog :: MVar Scene -> ErrorMV -> Chrono -> IO Watchdog
newWatchdog scmv errormv chrono = do
  dogmv <- forkWatchdog errormv hang >>= newMVar
  return $ \next -> do
    mth <- tryTakeMVar dogmv
    case mth of
      Nothing -> throwIO (bug "kick" "watchdog thread not found")
      Just th -> killThread th
    th <- forkWatchdog errormv (watchdogThread scmv chrono next)
    putMVar dogmv th

forkWatchdog :: ErrorMV -> IO () -> IO ThreadId
forkWatchdog errormv action = forkFinally action $ \result -> do
  case result of
    Left err -> case asyncExceptionFromException err of
      Nothing -> putMVar errormv ("watchdog", err)
      Just ThreadKilled -> return ()
      Just _ -> putMVar errormv ("watchdog", err)
    Right () -> return ()
    
watchdogThread :: MVar Scene -> Chrono -> Prediction ([Output],Scene) -> IO a
watchdogThread scmv chrono next = loop next where
  loop Never = hang
  loop (NotBefore dt) = do
    t0 <- chronoGetCurrent chrono
    let t1 = t0 + dt
    longSleep (nanosToMicros dt)
    next <- modifyMVar scmv $ \sc -> do
      dt <- chronoDiff chrono t1
      chronoSetTime chrono t1
      let !sc' = elapse sc dt
      let next = detect sc'
      return (sc', next)
    loop next
  loop (InExactly dt (outs, sc')) = do
    t0 <- chronoGetCurrent chrono
    let t1 = t0 + dt
    longSleep (nanosToMicros dt)
    next <- modifyMVar scmv $ \sc -> do
      chronoSetTime chrono t1
      let next = detect sc'
      putStrLn "watchdog outputting"
      forM_ outs execOutput
      return (sc', next)
    loop next

nanosToMicros n = n `div` 1000
