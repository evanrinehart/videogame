module Types where

import SDL
import Data.HashMap.Strict (HashMap)
import Data.Word
import Control.Concurrent
import Control.Exception

type Delta = Integer
type SampleRate = Integer
type ObjectId = Int

type ErrorMV = MVar (String, SomeException)

data RawInput =
  RawJoy !PlayerNo !CardinalDir !InputMotion |
  RawJump !PlayerNo !InputMotion |
  RawPlayersButton !Int !InputMotion |
  RawInsertCoin
    deriving (Show,Eq)

data Prediction a =
  Never | NotBefore Delta | InExactly Delta a
    deriving (Show)

instance Eq (Prediction a) where
  Never == Never = True
  NotBefore x == NotBefore y = x == y
  InExactly x _ == InExactly y _ = x == y
  _ == _ = False

instance Ord (Prediction a) where
  compare Never Never = EQ
  compare Never _ = GT
  compare (NotBefore x) (NotBefore y) = compare x y
  compare (NotBefore x) (InExactly y _) = compare x y
  compare (InExactly x _) (InExactly y _) = compare x y
  compare arg1 arg2 = compare EQ (compare arg2 arg1)

data CardinalDir = North | South | East | West deriving (Show, Eq)
data PlayerNo = Player1 | Player2 deriving (Show, Eq)

type Color = V3 Word8

data Input = Inp !RawInput
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

type Lens a b = (b -> b) -> a -> a
