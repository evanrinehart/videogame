module System where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad

import Types
import Credits
import Audio
import Graphics
import HighScore

data System = Sys
  { sysVideoOut      :: VideoOut
  , sysCtrlIn        :: ControlIn
  , sysSoundCtrl     :: SoundCtrlOut
  , sysCreditsVar    :: TVar Integer    -- r/w
  , sysHighScoresVar :: TVar HighScores -- read only
  , sysCommitScore   :: CommitScore }

type VideoOut     = TMVar Picture
type ControlIn    = TChan Input
type SoundCtrlOut = TChan SoundCtrl
type CommitScore  = TMVar ValidEntry

