module System where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.IORef

import Types
import Credits
import Audio
import Graphics
import HighScore

data System = Sys
  { sysVideoOut      :: VideoOut
  , sysCtrlIn        :: ControlIn
  , sysSoundCtrl     :: SoundCtrlOut
  , sysCreditsVar    :: TVar Integer
  , sysHighScores    :: STM HighScores
  , sysCommitScore   :: ValidEntry -> IO CommitStatus }

type ControlIn    = Chan Input
type SoundCtrlOut = TChan SoundCtrl

type VideoOut = (IORef Picture, MVar Picture)

newVideoOut :: IO VideoOut
newVideoOut = do
  mv <- newEmptyMVar
  ref <- newIORef Blank
  return (ref, mv)

writeVideo :: VideoOut -> Picture -> IO ()
writeVideo (ref, mv) = putMVar mv

readVideo :: VideoOut -> IO Picture
readVideo (ref, mv) = do
  r <- tryTakeMVar mv
  case r of
    Nothing -> readIORef ref
    Just p -> do
      writeIORef ref p
      return p
