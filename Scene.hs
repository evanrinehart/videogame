module Scene where

import Types
import Common
import Graphics
import SDL (InputMotion(Pressed))

import Data.Char
import Numeric

import Linear

data Scene = Scene
  { elapse :: Delta -> Scene
  , poke   :: Input -> ([Output], Scene)
  , detect :: Prediction ([Output], Scene)
  , view   :: Picture
  }

dummyScene :: Scene
dummyScene = Scene el po de vi where
  el _ = dummyScene
  po _ = ([], dummyScene)
  de = Never
  vi = Blank

counterScene :: Integer -> Scene
counterScene c = Scene el po de vi where
  el dt = counterScene (c + dt)
  po _ = ([], counterScene c)
  de = Never
  vi = gfxText (showIntAtBase 2 intToDigit (c `div` 100000000) "")

sequenceScene :: [Delta] -> Scene
sequenceScene [] = dummyScene
sequenceScene (c:cs) = Scene el po de vi where
  el dt | dt < c = sequenceScene (c-dt : cs)
        | otherwise = error "shouldn't happen, missed an occurrence"
  po _ = ([], dummyScene)
  de = InExactly c ([PlaySound 0], sequenceScene cs)
  vi = Blank

seqTest = sequenceScene [0, ms 100, ms 100, ms 100, ms 1000, ms 1000]

controlScene :: Integer -> Scene
controlScene c = Scene el po de vi where
  el _ = controlScene c
  po occ = case occ of
    Inp (RawJoy Player1 North Pressed) -> ([], controlScene (c + 1))
    Inp (RawJoy Player1 South Pressed) -> ([], controlScene (c - 1))
    _ -> ([], controlScene c)
  de = Never
  vi = gfxText (showIntAtBase 2 intToDigit c "")

splashScene :: Picture -> Scene
splashScene img = splashLoop (ms 2500) (Just ()) where
  splashLoop c pop = Scene el po de vi where
    el dt = splashLoop (c - dt) pop
    po _ = ([], splashLoop c pop)
    de = case pop of
      Nothing -> InExactly c ([], dummyScene)
      Just () -> InExactly 0 ([PlaySound 0], splashLoop c Nothing)
    vi = img

pokes :: Input -> [Scene] -> ([Output], [Scene])
pokes occ scs = go scs id [] where
  go [] outs scs' = (outs [], reverse scs')
  go (sc:scs) outsAcc scs' =
    let (outs, sc') = poke sc occ in
    go scs (outsAcc . (outs ++)) (sc':scs')

views :: [Scene] -> Picture
views = mconcat . map view 

elapses :: Delta -> [Scene] -> [Scene]
elapses dt = map (\sc -> elapse sc dt)

