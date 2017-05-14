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
import HighScore
import Credits

type Watchdog i a = Prediction ([Output], MainScene) -> IO ()

newWatchdog :: MVar MainScene -> ErrorMV -> Chrono -> HighScoreCache -> CreditsCounter -> IO (Watchdog i a)
newWatchdog scmv errormv chrono hsCache crCounter = do
  dogmv <- forkWatchdog errormv hang >>= newMVar
  return $ \next -> do
    mth <- tryTakeMVar dogmv
    case mth of
      Nothing -> throwIO (bug "kick" "watchdog thread not found")
      Just th -> killThread th
    th <- forkWatchdog errormv (watchdogThread scmv chrono hsCache crCounter next)
    putMVar dogmv th

forkWatchdog :: ErrorMV -> IO () -> IO ThreadId
forkWatchdog errormv action = forkFinally action $ \result -> do
  case result of
    Left err -> case asyncExceptionFromException err of
      Nothing -> putMVar errormv ("watchdog", err)
      Just ThreadKilled -> return ()
      Just _ -> putMVar errormv ("watchdog", err)
    Right () -> return ()
    
watchdogThread ::
  MVar MainScene ->
  Chrono ->
  HighScoreCache ->
  CreditsCounter ->
  Prediction ([Output], MainScene) ->
  IO b
watchdogThread scmv chrono hsCache crCounter next = loop next where
  loop Never = hang
  loop (NotBefore dt) = do
    t0 <- chronoGetCurrent chrono
    let t1 = t0 + dt
    longSleep (nanosToMicros dt)
    next <- modifyMVar scmv $ \sc -> do
      dt <- chronoDiff chrono t1
      chronoSetTime chrono t1
      hs <- getHighScores hsCache
      cr <- getCredits crCounter
      let !sc' = elapse sc (cr,hs) dt
      let next = detect sc' (cr,hs)
      return (sc', next)
    loop next
  loop (InExactly dt (outs, sc')) = do
    t0 <- chronoGetCurrent chrono
    let t1 = t0 + dt
    longSleep (nanosToMicros dt)
    next <- modifyMVar scmv $ \sc -> do
      chronoSetTime chrono t1
      hs <- getHighScores hsCache
      cr <- getCredits crCounter
      let next = detect sc' (cr,hs)
      putStrLn "watchdog outputting"
      forM_ outs (execOutput hsCache crCounter)
      return (sc', next)
    loop next

nanosToMicros n = n `div` 1000
