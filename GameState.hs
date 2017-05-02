module GameState where

import Linear (V2(..))

import Types
import Animation
import Graphics

import System.Random
import Control.Monad
import Control.Monad.Random

data Character = Marat | Longin
  deriving Show

type Game = GameState

data GameState = GameState
  { gsCredits :: Integer
  , gsHighScores :: [(String, Integer)]
  , gsMode :: GameMode
  }

data GameMode =
  ModeDeadAir Delta GameMode |
  ModeSplash Delta |
  ModeTitle (Animation Picture) |
  ModeHighScores Delta |
  ModeEnterHighScore EnterHighScoreScreen |
  ModeGameplay GameCore

data EnterHighScoreScreen = EnterHighScoreScreen
  { ehsCursor :: V2 Integer
  , ehsQueue :: [(Character, Integer)]
  , ehsName :: String
  , ehsConfirming :: Maybe Delta
  , ehsBlinker :: Animation Color
  }

data GameCore = GameCore
  { gcPlatforms :: [Platform]
  , gcScores    :: (V2 Integer)
  , gcLives     :: (V2 Integer)
  , gcPlayer1   :: (Maybe Player)
  , gcPlayer2   :: (Maybe Player)
  , gcPlayerX   :: (Maybe Player)
  , gcEnemies   :: [Enemy]
  , gcEnemyGen  :: (Delta, [Enemy])
  , gcCoins     :: [Coin]
  , gcPow       :: Integer
  , gcTreasure  :: (Maybe Treasure)
  , gcPipes     :: (V2 Pipe)
  , gcLimbo     :: [(Enemy, Side)]
  , gcSmoke     :: [Animation Picture]
  , gcLevelNo   :: Integer
  , gcTest      :: [(Int,Int,Bool)]
  , gcRng       :: StdGen
  , gcTest2     :: Bool
  }

data Coin = Coin
data Enemy = Enemy
data Side = Side
data Pipe = Pipe
data Treasure = Treasure
data Player = Player
data Platform = Platform


-- load an image.
-- make graphics API to display text
-- make an animation which cycles through 0 1 2 3
-- render the animation as numeric text
--

randomGrid :: StdGen -> ([(Int,Int,Bool)], StdGen)
randomGrid g = flip runRand g $ forM [(i,j)|i<-[0..39],j<-[0..29]] $ \(i,j) -> do
  x <- getRandom
  return (i,j,x)
