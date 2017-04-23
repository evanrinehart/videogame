module Engine where

import SDL (InputMotion)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Types
import Rails
import Game

-- assuming no events occur dt from now, advance all animations and timers
passTime :: Delta -> Game -> Game
passTime dt g@(Game rails _ credits) = g
  { gRails = fmap (advanceRail dt) rails
  }

-- collect all matching events and wake up sleeping scripts for execution
-- return the updated game and whatever "fire-and-forget IO effects"
poke :: Occur -> Game -> (Game, IO ())
poke occ g = (g, return ())

-- search for the soonest 
detectNext :: Game -> Prediction
detectNext g = Never

-- display the current state of the game
view :: Game -> Picture
view g = Blank

