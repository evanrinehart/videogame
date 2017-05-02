{-# LANGUAGE GADTSyntax #-}
module Graphics where

import Data.Monoid ((<>))
import Linear (V2(..), V3(..), V4(..))

import Types

data Picture where
  Blank  :: Picture
  Block  :: !(V4 Int) -> !Color -> Picture
  Sprite :: !(V4 Int) -> Picture
  Shift  :: !(V2 Int) -> Picture -> Picture
  Layer  :: Picture -> Picture -> Picture
    deriving (Show)

instance Monoid Picture where
  mempty = Blank
  mappend = Layer

letterWidth :: Char -> Int
letterWidth c = case c of
  '0' -> 7
  '1' -> 7
  _ -> error ("letter width: bad letter (" ++ show c ++ ")")

letterSrc :: Char -> V2 Int
letterSrc c = case c of
  '0' -> V2 0 0
  '1' -> V2 8 0
  _ -> error ("letter source: bad letter (" ++ show c ++ ")")

gfxText :: String -> Picture
gfxText str = go 0 str where
  go x "" = Blank
  go x (c:cs) =
    let w = letterWidth c in
    let V2 sx sy = letterSrc c in
    Shift (V2 x 0) (Sprite (V4 sx sy w 7)) <> go (x + w + 1) cs

-- colors
black    = V3 0 0 0
offWhite = V3 243 241 244
gray     = V3 151 146 169
darkGray :: Color
darkGray = V3 80 80 98
darkness = V3 34 36 40
darkBlue = V3 41 88 166
water    = V3 54 154 198
skyBlue  = V3 141 210 213
peach    = V3 252 217 196
pink     = V3 245 161 209
blood    = V3 204 97 101
brown    = V3 108 66 50
orange   = V3 208 123 58
gold     = V3 240 197 122
grass    = V3 131 156 49
cucumber = V3 43 96 66
