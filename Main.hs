{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (when, forever, forM_)  
import Control.Concurrent 
import Control.Exception
import System.Exit (exitSuccess)
import Data.IORef
import Foreign.C.Types

import SDL

import Types
import Common
import Engine
import Watchdog
import LongSleep
import Chrono
import Game
import Audio
import GameState
import Graphics
import Scene
import Output
import Renderer
import Error

import System.IO

main :: IO ()
main = do
  (renderer, scmv, kickDog, chrono, sprites, errormv) <- setup
  forever $ do
    checkForError errormv
    picture <- modifyMVar scmv $ \sc -> do
      -- advance scene and exec spontaneous outputs
      now <- chronoGetCurrent chrono
      dt <- chronoDiff chrono now
      chronoSetTime chrono now
      !sc' <- chewSceneTime sc dt
      -- process events
      evs <- pollEvents
      let (rawIns, done) = translateEvents evs
      sc'' <- chewSceneInputs sc' rawIns
      -- if one of the events was to quit, quit
      when done exitSuccess
      -- reset the watchdog thread
      kickDog (detect sc'')
      return (sc'', view sc'')
    -- generate a rendering and wait for vsync to show it
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer
    renderPicture renderer sprites picture
    present renderer 

translateEvents :: [Event] -> ([RawInput], Bool)
translateEvents evs = go evs [] False where
  go [] accum !done = (reverse accum, done)
  go (e:es) accum !done = case eventPayload e of
    KeyboardEvent (KeyboardEventData _ mo False (Keysym _ code _)) ->
      case keyToRaw code mo of
        Right raw -> go es (raw:accum) done
        Left esc -> go es accum (done || esc)
    QuitEvent -> go es accum True
    _ -> go es accum done

keyToRaw c mo = case c of
  KeycodeA -> Right (RawJoy Player1 West mo)
  KeycodeS -> Right (RawJoy Player1 South mo)
  KeycodeD -> Right (RawJoy Player1 East mo)
  KeycodeW -> Right (RawJoy Player1 North mo)
  KeycodeF -> Right (RawJoy Player2 West mo)
  KeycodeG -> Right (RawJoy Player2 South mo)
  KeycodeH -> Right (RawJoy Player2 East mo)
  KeycodeT -> Right (RawJoy Player2 North mo)
  KeycodeJ -> Right (RawJump Player1 mo)
  KeycodeK -> Right (RawJump Player2 mo)
  Keycode1 -> Right (RawPlayersButton 1 mo)
  Keycode2 -> Right (RawPlayersButton 2 mo)
  Keycode4 -> if (mo == Released) then Right RawInsertCoin else Left False
  KeycodeEscape -> Left True
  _ -> Left False

chewSceneTime :: Scene -> Delta -> IO Scene
chewSceneTime sc dt = case detect sc of
  Never -> do
    let !sc' = elapse sc dt
    return sc'
  NotBefore dt' -> if dt < dt'
    then do
      let !sc' = elapse sc dt
      return sc'
    else do
      let !sc' = elapse sc dt'
      chewSceneTime sc' (dt - dt')
  InExactly dt' (outs, sc') -> if dt < dt'
    then do
      let !sc'' = elapse sc dt
      return sc''
    else do
      putStrLn "main (time) outputting"
      forM_ outs execOutput
      chewSceneTime sc' (dt - dt')

chewSceneInputs :: Scene -> [RawInput] -> IO Scene
chewSceneInputs sc [] = return sc
chewSceneInputs sc (i:is) = do
  print i
  let !(outs, !sc') = poke sc (Inp i)
  putStrLn "main (input) outputting"
  forM_ outs execOutput
  chewSceneInputs sc' is

setup :: IO (Renderer, MVar Scene, Watchdog, Chrono, Texture, ErrorMV)
setup = do
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
  -- initial scene
  let sc = gameScene initialGameState
  scmv <- newMVar sc
  -- time regulator
  chrono <- newChrono 
  -- watchdog
  errormv <- newEmptyMVar
  kickDog <- newWatchdog scmv errormv chrono
  -- exception mvar
  return (renderer, scmv, kickDog, chrono, sprites, errormv)

checkForError :: ErrorMV -> IO ()
checkForError mv = do
  maybeError <- tryTakeMVar mv
  case maybeError of
    Just (who,err) -> throwIO (videoGameError who err)
    Nothing -> return ()
