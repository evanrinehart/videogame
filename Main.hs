{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when, forever, forM_)  
import Control.Concurrent (MVar, forkIO, newMVar, modifyMVar_, modifyMVar)
import System.Exit (exitSuccess)
import Data.IORef
import Foreign.C.Types

import SDL

import Types
import Engine
import Watchdog
import LongSleep
import Delta
import Game
import Audio
import GameState
import Graphics

main :: IO ()
main = do
  let sampleRate = 44100
  (renderer, gmv, kickDog, since, sprites) <- setup sampleRate
  appLoop gmv renderer sprites since $ \rawIn -> do
    print rawIn
    modifyMVar_ gmv $ \g -> do -- an input event has occurred
      let (g', output) = poke (OccRawIn rawIn) g
      let next = detect g'
      --output
      kickDog next
      return g'

appLoop :: MVar Game -> Renderer -> Texture -> IO Integer -> (RawInput -> IO ()) -> IO ()
appLoop gmv renderer sprites since deliver = forever $ do
  -- check for events
  evs <- pollEvents
  forM_ evs $ \e -> case eventPayload e of
    KeyboardEvent (KeyboardEventData _ mo False (Keysym _ code _)) -> case code of
      KeycodeEscape -> exitSuccess
      KeycodeA -> deliver (RawJoy Player1 West mo)
      KeycodeS -> deliver (RawJoy Player1 South mo)
      KeycodeD -> deliver (RawJoy Player1 East mo)
      KeycodeW -> deliver (RawJoy Player1 North mo)
      KeycodeF -> deliver (RawJoy Player2 West mo)
      KeycodeG -> deliver (RawJoy Player2 South mo)
      KeycodeH -> deliver (RawJoy Player2 East mo)
      KeycodeT -> deliver (RawJoy Player2 North mo)
      KeycodeJ -> deliver (RawJump Player1 mo)
      KeycodeK -> deliver (RawJump Player2 mo)
      Keycode1 -> deliver (RawPlayersButton 1 mo)
      Keycode2 -> deliver (RawPlayersButton 2 mo)
      Keycode4 -> when (mo == Pressed) (deliver RawInsertCoin)
      _ -> return ()
    QuitEvent -> exitSuccess
    _ -> return ()

  -- generate a rendering an wait for vsync to show it
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  picture <- modifyMVar gmv $ \g -> do
    dt <- since
    let g' = elapse dt g 
    return (g', view g')
  renderPicture renderer sprites picture
  present renderer 

renderPicture :: Renderer -> Texture -> Picture -> IO ()
renderPicture rend sprites pic = go pic (V2 0 0) where
  go p o@(V2 ox oy) = case p of
    Blank -> return ()
    Block (V4 x y w h) (V3 r g b) -> do
      let x' = x + ox
      let y' = y + oy
      rendererDrawColor rend $= V4 r g b 255
      case rectSplit (V4 x' y' w h) of
        NoSplit -> fillRect rend (Just (mkRect x' y' w h))
        SideSplit w1 w2 h -> do
          fillRect rend (Just (mkRect x' y' w1 h))
          fillRect rend (Just (mkRect 0 y' w2 h))
        TopSplit h1 h2 w -> do
          fillRect rend (Just (mkRect x' y' w h1))
          fillRect rend (Just (mkRect x' 0 w h2))
        QuadSplit w1 w2 h1 h2 -> do
          fillRect rend (Just (mkRect x' y' w1 h1))
          fillRect rend (Just (mkRect 0 y' w2 h1))
          fillRect rend (Just (mkRect x' 0 w1 h2))
          fillRect rend (Just (mkRect 0 0 w2 h2))
    Sprite (V4 sx sy w h) -> case rectSplit (V4 ox oy w h) of
      NoSplit -> do
        let source = mkRect sx sy w h
        let dest = mkRect ox oy w h
        copy rend sprites (Just source) (Just dest)
      _ -> error "split"
    Shift shift p' -> go p' (o + shift)
    Layer p1 p2 -> do
      go p1 o
      go p2 o

convRect :: V4 Int -> Rectangle CInt
convRect (V4 x y w h) = mkRect x y w h

mkRect :: Int -> Int -> Int -> Int -> Rectangle CInt
mkRect x y w h = Rectangle p (fmap fromIntegral (V2 w h)) where
  p = P (fmap fromIntegral (V2 x y))
    
data RectSplit =
  NoSplit |
  SideSplit !Int !Int !Int |
  TopSplit !Int !Int !Int |
  QuadSplit !Int !Int !Int !Int 
    deriving Show

rectSplit :: V4 Int -> RectSplit
rectSplit r@(V4 x y w h)
  | (x + w) `mod` screenW < x && (y + h) `mod` screenH < y =
      let w1 = screenW - x
          w2 = x + w - screenW
          h1 = screenH - y
          h2 = y + h - screenH
       in QuadSplit w1 w2 h1 h2
  | (x + w) `mod` screenW < x =
      let w1 = screenW - x
          w2 = x + w - screenW
       in SideSplit w1 w2 h
  | (y + h) `mod` screenH < y =
      let h1 = screenH - y
          h2 = y + h - screenH
       in TopSplit h1 h2 w
  | otherwise = NoSplit

screenW :: Int
screenW = 320
screenH :: Int
screenH = 240

setup :: SampleRate -> IO (Renderer, MVar Game, Watchdog, IO Integer, Texture)
setup sampleRate = do
  -- sdl
  initializeAll
  -- video
  window <- createWindow
    "VIDEO GAME"
    (defaultWindow
      { windowInitialSize=V2 (fromIntegral screenW) (fromIntegral screenH)
      , windowMode=Windowed
      })
  renderer <- createRenderer
    window
    (-1)
    (defaultRenderer {rendererType=AcceleratedVSyncRenderer})
  spriteSurf <- loadBMP "sprites.bmp"
  sprites <- createTextureFromSurface renderer spriteSurf
  freeSurface spriteSurf
  -- audio
  (device, _) <- openAudioDevice (OpenDeviceSpec
    { openDeviceFreq = Mandate (fromInteger sampleRate)
    , openDeviceFormat = Mandate Signed16BitLEAudio
    , openDeviceChannels = Mandate Mono
    , openDeviceSamples = 2048
    , openDeviceCallback = audioCallback 
    , openDeviceUsage = ForPlayback
    , openDeviceName = Nothing
    })
  setAudioDevicePlaybackState device Play
  -- central game state
  let g = initialGameState
  gmv <- newMVar g
  -- watchdog
  kickDog <- newWatchdog gmv
  let next = detect g
  kickDog next 
  -- time delta generator
  since <- newDeltaGenerator sampleRate
  return (renderer, gmv, kickDog, since, sprites)
