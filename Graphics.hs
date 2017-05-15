{-# LANGUAGE GADTSyntax #-}
module Graphics where

import Data.Monoid ((<>))
import Data.Char

import Linear

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
letterWidth c
  | c == 'I' = 4
  | c == 'T' = 6
  | c == 'L' = 6
  | c == 'Y' = 6
  | c == ')' = 5
  | c == '(' = 5
  | c == ':' = 4
  | c == '.' = 4
  | otherwise = 7
--  _ -> error ("letter width: bad letter (" ++ show c ++ ")")

le row col = V2 (col * 8) (16 + row * 8)
nu n = V2 (n * 8) 0

letterSrc :: Char -> V2 Int
letterSrc c
  | c >= '0' && c <= '9' = nu (ord c - ord '0')
  | c >= 'A' && c <= 'Z' = let (q,r) = divMod (ord c - ord 'A') 8 in le q r
  | c == '(' = le 3 4
  | c == ')' = le 3 5
  | c == ':' = le 3 6
  | c == '.' = le 3 7
  | c == '#' = le 4 0
  | c == ' ' = le 3 2
  | otherwise = le 3 3

gfxText :: String -> Picture
gfxText str = go 0 (map toUpper str) where
  go x "" = Blank
  go x (c:cs) =
    let w = letterWidth c in
    let V2 sx sy = letterSrc c in
    Shift (V2 x 0) (Sprite (V4 sx sy w 7)) <> go (x + w + 1) cs

-- colors
black,offWhite,gray,darkGray,
  darkness,darkBlue,water,skyBlue,
  peach,pink,blood,brown,
  orange,gold,grass,cucumber :: Color
black    = V3 0 0 0
offWhite = V3 243 241 244
gray     = V3 151 146 169
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

