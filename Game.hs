module Game where

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashMap.Strict as HM
import Linear (V2(..))
import System.Random
import Control.Monad
import Control.Monad.Random

import Types
import Rails
import GameState

initialHighScores :: [(String, Integer)]
initialHighScores =
  [("BIF", 69696)
  ,("BEJ", 50000)
  ,("NER", 40000)
  ,("ENE", 30000)
  ,("VIN", 20000)]

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
  , gcTest = []
  , gcRng = mkStdGen 0
  , gcTest2 = False
  }
