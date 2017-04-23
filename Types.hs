module Types where

import SDL
import Data.HashMap.Strict (HashMap)

import Common

type Delta = Integer
type ObjectId = Int

data RawInput =
  RawJoy PlayerNo CardinalDir InputMotion |
  RawJump PlayerNo InputMotion |
  RawPlayersButton Int InputMotion |
  RawInsertCoin
    deriving (Show,Eq)

data Prediction =
  Never | NotBefore Delta | InExactly Delta Occur
    deriving (Show, Eq)

data CardinalDir = North | South | East | West deriving (Show, Eq)
data PlayerNo = Player1 | Player2 deriving (Show, Eq)

data Picture = Blank
data Occur = OccRawIn RawInput deriving (Show, Eq)
