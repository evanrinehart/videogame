module Engine where

import SDL (InputMotion)

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Linear
import Debug.Trace

import Data.Char
import Numeric

import Types
import Rails
import GameState
import Graphics

-- assuming no events occur dt from now, advance all animations and timers
{-
elapse :: Delta -> Game -> Game
elapse dt = gsMode' f where
  f (ModeDeadAir timeLeft gc') | dt < timeLeft = ModeDeadAir (timeLeft - dt) gc'
                               | otherwise = elapse (dt - timeLeft) gc'
  f (ModeSplash timeLeft) | dt < timeLeft = ModeSplash (timeLeft - dt)
                          | otherwise = ModeDeadAir (ms 250) titleScreen
  ModeTitle (Animation Picture) |
  ModeHighScores Delta |
  ModeEnterHighScore EnterHighScoreScreen |
  ModeGameplay GameCore
  f (ModeGameplay gc) = ModeGameplay $
    let c = dt + gcTest2 gc in
    let (steps, c') = c `divMod` 4000 in
    if steps > 0
      then
        let (grid, gc') = runGameRandom randomGrid gc in
        gc' { gcTest2 = c', gcTest = grid }
      else
        gc { gcTest2 = c' }
-}

debug :: Show a => a -> a
debug x = Debug.Trace.trace (show x) x
    
{-
  | gcTest2 c < dt =
      let (grid, rng') = randomGrid (gcRng c) in
      GameState a b (ModeGameplay (c { gcTest = grid, gcRng = rng', gcTest2 = gcTest2 c}))
  else
    GameState a b (ModeGameplay (c { gcTest2 = gcTest2 c - dt }))
-}

-- collect all matching events and wake up sleeping scripts for execution
-- return the updated game and whatever "fire-and-forget IO effects"
--poke :: Occur -> Game -> (Game, [Output])
--poke occ g = (g, [])

-- search for the soonest 
{-
detect :: Game -> Prediction
detect gs = case gsMode gs of
-}
  
-- display the current state of the game
{-
view :: Game -> Picture
view (GameState credits highScores mode) = case mode of
  ModeGameplay core -> mconcat (map f (gcTest core)) where
    f (i,j,bool) = Shift (V2 (i*8) (j*8)) (gfxText (if bool then "1" else "0"))
  _ -> Blank
-}
