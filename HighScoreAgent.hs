module HighScoreAgent where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import System.Timeout

import Util
import Audio
import LongSleep
import System
import Debug
import HighScore

-- high score agent updates the high scores and reacts to problems
-- committing them to the file system

highScoreAgent :: TVar HighScores -> CommitChan -> SoundCtrlOut -> FilePath -> IO a
highScoreAgent hsVar commitCh soundCtrl dirname = mode1 where
  listen = do
    (entry, retMV) <- takeMVar commitCh
    scores <- atomically $ do
      modifyTVar hsVar (updateHighScores entry)
      readTVar hsVar
    return (scores, retMV)
  mode1 = do -- normal mode
    (scores, retMV) <- listen
    mode1' retMV scores
  mode1' retMV scores = do
    saving <- async (saveScores dirname scores)
    result <- timeout 100000 (waitCatch saving)
    case result of
      Nothing -> do
        cancel saving
        logMsg "highscores" "Saving scores taking too long. Deploying doctor."
        mode1'' retMV
      Just (Left err) -> do
        logErr "highscores" "FAILED" (show err)
        logMsg "highscores" "Saving scores failed. Deploying doctor."
        mode1'' retMV
      Just (Right _) -> do
        atomically $ writeTChan soundCtrl (PlaySound RegistrateSound)
        putMVar retMV CommitOK
        mode1
  mode1'' retMV = do
    putMVar retMV CommitIncomplete
    treatment <- async doctor
    mode2 treatment
  mode2 treatment = do -- getting serious
    (scores, retMV) <- listen
    status <- pollThrow treatment
    case status of
      Nothing -> do
        putMVar retMV CommitIncomplete
        mode2 treatment
      Just () -> do
        logMsg "highscores" "High score saving seems to have recovered."
        mode1' retMV scores
  doctor = do
    logMsg "highscores:doctor" "I will attempt to save the latest high scores until it actually works. Stand by... "
    unlimitedExponentialRetrier (logErr "highscores:doctor" "ERROR") $ do
      scores <- atomically (readTVar hsVar)
      saveScores dirname scores
      logMsg "highscores:doctor" "High score saving successful."
