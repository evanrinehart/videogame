module Engine where

import SDL (InputMotion)

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Linear

import Data.Char
import Numeric

import Types
import Rails
import GameState
import Graphics

-- assuming no events occur dt from now, advance all animations and timers
elapse :: Delta -> Game -> Game
elapse dt (GameState a b (ModeGameplay c)) = if gcTest2 c
  then
    let (grid, rng') = randomGrid (gcRng c) in
    GameState a b (ModeGameplay (c { gcTest = grid, gcRng = rng', gcTest2 = False}))
  else
    GameState a b (ModeGameplay (c { gcTest2 = True }))

-- collect all matching events and wake up sleeping scripts for execution
-- return the updated game and whatever "fire-and-forget IO effects"
poke :: Occur -> Game -> (Game, [Output])
poke occ g = (g, [])

-- search for the soonest 
detect :: Game -> Prediction
detect g = Never

-- display the current state of the game
view :: Game -> Picture
view (GameState credits highScores mode) = case mode of
  ModeGameplay core -> mconcat (map f (gcTest core)) where
    f (i,j,bool) = Shift (V2 (i*8) (j*8)) (gfxText (if bool then "1" else "0"))
  _ -> Blank
