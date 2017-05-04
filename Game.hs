module Game where

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashMap.Strict as HM
import Linear (V2(..))
import System.Random
import Control.Monad
import Control.Monad.Random

import Types
import Common
import Rails
import GameState
import Graphics
import Scene

initialHighScores :: [(String, Integer)]
initialHighScores =
  [("BIF", 50000)
  ,("BEJ", 40000)
  ,("NER", 30000)
  ,("ENE", 20000)
  ,("VIN", 10000)]

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

gameScene :: GameState -> Scene
gameScene gs = Scene el po de vi where
  el dt = gameScene gs
  po occ = ([], gameScene gs)
  de = Never
  vi = Blank
