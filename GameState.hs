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

gsCredits' f (GameState x y z) = GameState (f x) y z
gsHighScores' f (GameState x y z) = GameState x (f y) z
gsMode' f (GameState x y z) = GameState x y (f z)

data GameMode =
  ModeDeadAir Delta GameMode |
  ModeSplash Delta (Maybe ()) |
  ModeTitle (Animation Picture) |
  ModeHighScores Delta |
  ModeEnterHighScore EnterHighScoreScreen |
  ModeGameplay GameCore

modeGameplay' f (ModeGameplay gc) = ModeGameplay (f gc)
modeGameplay' f mode = mode

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
  , gcRng       :: StdGen
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

runGameRandom :: Rand StdGen a -> GameCore -> (a, GameCore)
runGameRandom act gc =
  let rng = gcRng gc in
  let (x, rng') = runRand act rng in
  (x, gc { gcRng = rng' })

randomGrid :: Rand StdGen [(Int,Int,Bool)]
randomGrid = forM [(i,j)|i<-[0..39],j<-[0..29]] $ \(i,j) -> do
  x <- getRandom
  return (i,j,x)

