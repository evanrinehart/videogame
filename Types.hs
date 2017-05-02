module Types where

import SDL
import Data.HashMap.Strict (HashMap)
import Data.Word

type Delta = Integer
type SampleRate = Integer
type ObjectId = Int

data RawInput =
  RawJoy !PlayerNo !CardinalDir !InputMotion |
  RawJump !PlayerNo !InputMotion |
  RawPlayersButton !Int !InputMotion |
  RawInsertCoin
    deriving (Show,Eq)

data Prediction =
  Never | NotBefore !Delta | InExactly !Delta !Occur
    deriving (Show, Eq)

data CardinalDir = North | South | East | West deriving (Show, Eq)
data PlayerNo = Player1 | Player2 deriving (Show, Eq)

type Color = V3 Word8

data Occur =
  OccRawIn !RawInput |
  OccEndOf !ObjectId |
  OccCollision !ObjectId !ObjectId
    deriving (Show, Eq)

data Rail = Rail
  { railValue :: !Integer
  , railSize :: !Integer
  , railCounter :: !Integer
  , railSpeed :: Integer -> Speed
  }

type Speed = Rational

data AddInf a = Inf | Fin a deriving (Show, Eq)

instance Ord a => Ord (AddInf a) where
  compare Inf Inf = EQ
  compare (Fin x) (Fin y) = compare x y
  compare Inf _ = GT
  compare _ Inf = LT

data Output =
  CommitHighScore String Integer |
  PlaySound Int
    deriving Show

{-
data Platform = Platform
  { platX :: (Integer, Integer)
  , platY :: Integer
  , platMaterial :: Integer -> Material
  }

data PlayerChar = PlayerChar
  { plLocX :: ObjectId
  , plLocY :: ObjectId
  , plSpr :: (ObjectId, Integer -> Picture)
  }

data BigLetter = BigLetter
  { blLetter :: Char
  , blColor :: (ObjectId, Integer -> Color)
  , blTransform :: ObjectId
  }

-- types that need fleshing out
data Game = Game
  { gSpace       :: !()
  , gRails       :: !(HashMap ObjectId Rail)
  , gPlatforms   :: !(HashMap ObjectId Platform)
  , gPlayers     :: !(HashMap ObjectId PlayerChar)
  , gObjectIdMax :: !Integer
  , gCredits     :: !Integer
  , gScore1      :: !Integer
  , gScore2      :: !Integer
  , gHighScores  :: ![([Char], Integer)]
  , gScripts     :: !(HashMap ScriptId Awaiting)
  , gChildren    :: !(HashMap ScriptId [ScriptId])
  , gClaims      :: !(HashMap ScriptId [ObjectId])
  }

data Viz a where
  VizRail :: ObjectId -> Viz Rail
  VizPlat :: ObjectId -> Viz Platform
  VizApp :: Viz (a -> b) -> Viz a -> Viz b
  VizPure :: a -> Viz a
  VizMap :: (a -> b) -> Viz a -> Viz b
-}  
