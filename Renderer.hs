module Renderer where

import SDL
import Linear
import Foreign.C

import Graphics

renderPicture :: Renderer -> Texture -> Picture -> IO ()
renderPicture rend sprites pic = go pic (V2 0 0) where
  go p o@(V2 ox oy) = case p of
    Blank -> return ()
    Block (V4 x y w h) (V3 r g b) -> do
      let x' = x + ox
      let y' = y + oy
      rendererDrawColor rend $= V4 r g b 255
      fillRect rend (Just (mkRect x' y' w h))
    Sprite (V4 sx sy w h) -> do
      let source = mkRect sx sy w h
      let dest = mkRect ox oy w h
      copy rend sprites (Just source) (Just dest)
    Shift shift p' -> go p' (o + shift)
    Layer p1 p2 -> do
      go p1 o
      go p2 o

mkRect :: Int -> Int -> Int -> Int -> Rectangle CInt
mkRect x y w h = Rectangle p (fmap fromIntegral (V2 w h)) where
  p = P (fmap fromIntegral (V2 x y))
