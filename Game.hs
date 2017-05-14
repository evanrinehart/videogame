{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Game where

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashMap.Strict as HM
import Linear (V2(..))
import System.Random
import Control.Monad
import Control.Monad.Random

import Debug.Trace (trace)

import Types
import Common
import Rails
import GameState
import Graphics
import Prog
import HighScore
import Script

debug :: Show a => String -> a -> a
debug msg x = trace (msg ++ "=" ++ show x) x

{-
initialGameState :: GameState
initialGameState = GameState 0 initialHighScores $ ModeGameplay $ GameCore
  { gcPlatforms = []
  , gcScores = V2 0 0
  , gcLives = V2 3 3
  , gcPlayer1 = Nothing
  , gcPlayer2 = Nothing
  , gcPlayerX = Nothing
  , gcEnemies = []
  , gcEnemyGen = (0, [])
  , gcCoins = []
  , gcPow = 3
  , gcTreasure = Nothing
  , gcPipes = V2 Pipe Pipe
  , gcLimbo = []
  , gcSmoke = []
  , gcLevelNo = 1
  , gcRng = mkStdGen 0
  }
-}


root :: Prog Output (Credits, HighScores) Input Picture
root = Prog st po vi where
  st r = ([], root)
  po r i = ([], root)
  vi r = Blank

