module Game where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Types
import Rails

-- types that need fleshing out
data Game = Game
  { gRails :: HashMap ObjectId Rail
  , gObjectIdMax :: Integer
  , gCredits :: Integer
  } deriving (Show)

initialGameState :: Game
initialGameState = Game
  { gRails = HM.fromList [(0, simpleRail 100 12)]
  , gObjectIdMax = 0
  , gCredits = 0
  }
