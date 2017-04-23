{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when, forever, forM_)  
import Control.Concurrent (MVar, forkIO, newMVar, modifyMVar_, modifyMVar)
import System.Exit (exitSuccess)
import Data.IORef

import SDL

import Types
import Engine
import Watchdog
import LongSleep
import Delta
import Game
import Audio

main :: IO ()
main = do
  let sampleRate = 44100
  (renderer, gmv, dogmv, since) <- setup sampleRate
  appLoop gmv renderer since $ \rawIn -> do
    print rawIn
    modifyMVar_ gmv $ \g -> do -- an input event has occurred
      let (g', output) = poke (OccRawIn rawIn) g
      let next = detectNext g'
      output
      kickWatchdog next gmv dogmv
      return g'

appLoop :: MVar Game -> Renderer -> IO Integer -> (RawInput -> IO ()) -> IO ()
appLoop gmv renderer since deliver = forever $ do
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
  clear renderer
  picture <- modifyMVar gmv $ \g -> do
    dt <- since
    let g' = passTime dt g 
    return (g', view g')
  renderPicture picture
  present renderer 

renderPicture :: Picture -> IO ()
renderPicture pic = do
  case pic of
    Blank -> return ()

setup sampleRate = do
  initializeAll
  window <- createWindow
    "VIDEO GAME"
    (defaultWindow {windowInitialSize=V2 320 240})
  renderer <- createRenderer
    window
    (-1)
    (defaultRenderer {rendererType=AcceleratedVSyncRenderer})
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
  let g = initialGameState
  gmv <- newMVar g
  dogmv <- forkIO hang >>= newMVar
  let next = detectNext g
  kickWatchdog next gmv dogmv
  since <- newDeltaGenerator sampleRate
  return (renderer, gmv, dogmv, since)

