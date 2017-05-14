{-# LANGUAGE MultiWayIf #-}
module Scene where

import Types
import Common
import Graphics
import SDL (InputMotion(Pressed))

import Data.Monoid
import Control.Applicative
import Data.Char
import Data.Maybe
import Numeric
import Varying

import Linear

data Scene o r i a = Scene
  { elapse :: Varying r -> Delta -> Scene o r i a
  , poke   :: r -> i -> ([o], Scene o r i a)
  , detect :: Varying r -> Prediction ([o], Scene o r i a)
  , view   :: Varying r -> Varying a
  }

type MainScene = Scene (Credits,HighScores) Input Output Picture

instance (Monoid r, Show a) => Show (Scene o r i a) where
  show sc = show (view sc mempty)

dummyScene :: Monoid a => Scene o r i a
dummyScene = Scene el po de vi where
  el _ _ = dummyScene
  po _ _ = ([], dummyScene)
  de _ = Never
  vi _ = mempty

instance Monoid a => Monoid (Scene o r i a) where
  mempty = dummyScene
  mappend = liftA2 (<>)

instance Functor (Scene o r i) where
  fmap f (Scene e p d v) = Scene e' p' d' v' where
    e' r dt = fmap f (e r dt)
    p' r i = fmap (fmap f) (p r i)
    d' r = fmap (fmap (fmap f)) (d r)
    v' r = fmap f (v r)

instance Applicative (Scene o r i) where
  pure x = fmap (const x) (mempty :: Scene r i o ())
  scf <*> scx = fmap (\(f,x) -> f x) (pairScene scf scx)

pairScene :: Scene o r i a -> Scene o r i b -> Scene o r i (a,b)
pairScene (Scene e1 p1 d1 v1) (Scene e2 p2 d2 v2) = Scene e3 p3 d3 v3 where
  e3 r dt = pairScene (e1 r dt) (e2 r dt)
  p3 r i =
    let (os1, sc1') = p1 r i in
    let (os2, sc2') = p2 r i in
    (os1++os2, pairScene sc1' sc2')
  d3 r =
    let x = d1 r in
    let y = d2 r in
    if | x `predLT` y -> case x of
          NotBefore dt -> NotBefore dt
          InExactly dt (os,sc') -> InExactly dt (os, pairScene sc' (e2 r dt))
       | y `predLT` x -> case y of
          NotBefore dt -> NotBefore dt
          InExactly dt (os,sc') -> InExactly dt (os, pairScene (e1 r dt) sc')
       | otherwise -> case (x,y) of
          (Never,Never) -> Never
          (NotBefore dt, NotBefore dt') -> NotBefore dt
          (InExactly dt (os1,sc1'), InExactly dt' (os2,sc2')) ->
            InExactly dt (os1++os2, pairScene sc1' sc2')
  v3 r = timeZip (,) (v1 r) (v2 r)

omap :: (o -> o') -> Scene o r i a -> Scene o' r i a
omap f (Scene e p d v) = Scene e' p' d' v' where
  e' r dt = omap f (e r dt)
  p' r i = let (outs, sc') = p r i in (map f outs, omap f sc')
  d' r = fmap (\(outs,sc') -> (map f outs, omap f sc')) (d r)
  v' = v

imap :: (i' -> i) -> Scene o r i a -> Scene o r i' a
imap f (Scene e p d v) = Scene e' p' d' v' where
  e' r dt = imap f (e r dt)
  p' r i = fmap (imap f) (p r (f i))
  d' r = fmap (fmap (imap f)) (d r)
  v' = v

justOut :: Scene (Maybe o) r i a -> Scene o r i a
justOut (Scene e p d v) = Scene e' p' d' v' where
  e' r dt = justOut (e r dt)
  p' r i = let (outs, sc') = p r i in (catMaybes outs, justOut sc')
  d' r = fmap (\(outs,sc') -> (catMaybes outs, justOut sc')) (d r)
  v' = v

-- Nothings don't affect the scene
-- Just i arrives as i without the Just
justIn :: Scene o r i a -> Scene o r (Maybe i) a
justIn sc@(Scene e p d v) = Scene e' p' d' v' where
  e' r dt = justIn (e r dt)
  p' r Nothing = ([], justIn sc)
  p' r (Just i) = fmap justIn (p r i)
  d' r = fmap (fmap justIn) (d r)
  v' = v 


identityScene :: Scene i a i a
identityScene = Scene el po de vi where
  el r dt = identityScene
  po r i = ([i], identityScene)
  de r = Never
  vi r = r

composeScene :: Scene o r i a -> Scene o' r' o r -> Scene o' r' i a
composeScene (Scene e1 p1 d1 v1) (Scene e2 p2 d2 v2) = Scene e3 p3 d3 v3 where
  e3 r' dt = 
    let sc2' = e2 r' dt in
    let sc1' = e1 (v2 r') dt in
    composeScene sc1' sc2'
  p3 r' i = undefined
  d3 r' = case comparePred (d2 r') (d1 (v2 r')) of
    LT -> undefined
    GT -> undefined
    EQ -> undefined
  v3 r' = v1 (v2 r')


