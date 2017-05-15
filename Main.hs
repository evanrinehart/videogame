{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (when, forever, forM_)  
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception
import System.Exit (exitSuccess, exitFailure)
import System.IO
import Data.IORef
import Data.List
import Data.Maybe
import Data.Monoid
import System.Mem
import Data.Typeable

import SDL

import Types
import Audio
import HighScore
import Credits
import TitleScene
import Graphics
import System
import Renderer
import Util
import Error
import Debug
import Splash
import LongSleep
import HighScoreAgent

sampleRate = 44100
audioBufferSize = 2048
(screenW, screenH) = (320, 240)

main :: IO ()
main = do
  initializeGlobalProgramStartTime
  (sdlRenderer, sprites, sys, hsAgent, logger) <- setup
  let videoOut = sysVideoOut sys
  let ctrlIn = sysCtrlIn sys
  let initialScene = splashScene sys
  let l = logMsg "main"
  l ""
  l "Monitoring critical agents."
  threads <- async $ spawnGroup
    [("credits-persist", creditsAgent sys)
    ,("coin-slot", coinAgent sys)
    ,("highscore-saver", wait hsAgent)
    ,("scene", sceneThread sys videoOut initialScene)
    ,("logger", wait logger)]
  l "Entering the main loop. Good luck!"
  performMajorGC
  forever $ do
    -- get events if any
    evs <- pollEvents
    let inputs = extractInputs evs
    -- push input events
    forM_ inputs (writeChan ctrlIn)
    -- if should exit, exit
    when (shouldQuit evs) $ do
      l "Got the signal to exit."
      logMsg "O_o" "Signing off."
      threadDelay 250000
      cancel logger
      hFlush stdout
      hFlush stderr
      putStr =<< getCurrentTimestamp
      putStrLn " **** exit ****"
      exitSuccess
    -- get a picture
    picture <- readVideo videoOut
    -- display the picture
    rendererDrawColor sdlRenderer $= V4 0 0 0 255
    clear sdlRenderer
    renderPicture sdlRenderer sprites picture
    -- block until displayed
    present sdlRenderer 
    -- see if any main threads crashes
    status <- poll threads
    case status of
      Just (Right (who, err)) -> do
        logErr who "FATAL" (show err)
        threadDelay 1000000
        throwIO err
      Just (Left err) -> do
        logErr "main" "FATAL" (show err)
        threadDelay 1000000
        throwIO err
      Nothing -> return ()

setup :: IO (Renderer, Texture, System, Async a, Async a)
setup = do
  logger <- async loggerAgent
  let l = logMsg "main"
  l "Begin boot-up sequence"
  -- "global" STM vars and chans
  videoOut <- newVideoOut
  ctrlIn <- newChan
  soundCtrl <- newTChanIO
  commitCh <- newEmptyMVar
  l "Restoring credits... "
  cr0 <- fetchCreditsCount >>= \case
    Left err -> logErr "main" "ERROR" (show err) >> return 0
    Right n  -> return n
  l ("   ... There are " ++ show cr0 ++ " credits in the machine.")
  creditsVar <- newTVarIO cr0
  let dbpath = "database"
  l "Recovering high scores... "
  scores <- fetchHighScores dbpath
  l ("   ... " ++ show scores)
  highScoresVar <- newTVarIO scores
  let commitAction entry = do
        ret <- newEmptyMVar
        putMVar commitCh (entry, ret)
        takeMVar ret
  let readHS = readTVar highScoresVar
  hsAgent <- async (highScoreAgent highScoresVar commitCh soundCtrl "database")
  let sys = Sys videoOut ctrlIn soundCtrl creditsVar readHS commitAction
  -- sdl
  l "Initializing SDL... "
  initializeAll
  l "   CHECK."
  -- video
  l "Creating window... "
  window <- createWindow
    "VIDEO GAME"
    (defaultWindow
      { windowInitialSize=V2 (fromIntegral screenW) (fromIntegral screenH)
      , windowMode=Windowed
      })
  l "   CHECK."
  l "Creating accelerated renderer... "
  renderer <- createRenderer
    window
    (-1)
    (defaultRenderer {rendererType=AcceleratedVSyncRenderer})
  l "   CHECK."
  l "Loading sprites... "
  spriteSurf <- loadBMP "sprites.bmp"
  l "   CHECK... "
  sprites <- createTextureFromSurface renderer spriteSurf
  freeSurface spriteSurf
  l "   CHECK."
  -- audio
  l $ 
    "Opening audio device {sampleRate = " ++ show sampleRate ++
    ", bufferSize = " ++ show audioBufferSize ++ "} ... "
  (device, _) <- openAudioDevice (OpenDeviceSpec
    { openDeviceFreq = Mandate (fromInteger sampleRate)
    , openDeviceFormat = Mandate Signed16BitLEAudio
    , openDeviceChannels = Mandate Mono
    , openDeviceSamples = audioBufferSize
    , openDeviceCallback = audioCallback soundCtrl
    , openDeviceUsage = ForPlayback
    , openDeviceName = Nothing
    })
  l "   CHECK..."
  setAudioDevicePlaybackState device Play
  l "   CHECK."
  -- initial scene
  return (renderer, sprites, sys, hsAgent, logger)

        
extractInputs :: [Event] -> [Input]
extractInputs (e:ev) = answer where
  answer = if isQuit e then [] else maybe next (\i -> i : next) mi 
  next = extractInputs ev
  mi = do
    (motion,code) <- case eventPayload e of
      KeyboardEvent (KeyboardEventData _ mo False (Keysym _ code _)) -> Just (mo,code)
      _ -> Nothing
    let act = case motion of {Pressed -> Press; Released -> Release}
    let js = Joystick
    case code of
      KeycodeA -> Just (js Player1 West act)
      KeycodeS -> Just (js Player1 South act)
      KeycodeD -> Just (js Player1 East act)
      KeycodeW -> Just (js Player1 North act)
      KeycodeF -> Just (js Player2 West act)
      KeycodeG -> Just (js Player2 South act)
      KeycodeH -> Just (js Player2 East act)
      KeycodeT -> Just (js Player2 North act)
      KeycodeJ -> Just (JumpButton Player1 act)
      KeycodeK -> Just (JumpButton Player2 act)
      Keycode1 -> case act of
        Press -> Just (StartButton Player1)
        _ -> Nothing
      Keycode2 -> case act of
        Press -> Just (StartButton Player2)
        _ -> Nothing
      Keycode4 -> case act of
        Press -> Just InsertCoin
        _ -> Nothing
      _ -> Nothing
extractInputs [] = []

isQuit :: Event -> Bool
isQuit e = case eventPayload e of
  KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeEscape _)) -> True
  QuitEvent -> True
  _ -> False

shouldQuit :: [Event] -> Bool
shouldQuit = any isQuit

creditsAgent :: System -> IO a
creditsAgent (Sys _ _ soundCtrl creditsVar _ _) = do
  c0 <- atomically (readTVar creditsVar)
  watchForChanges c0 creditsVar f where
    f c c' = when (c' > c) $ do
      atomically (writeTChan soundCtrl (PlaySound CoinSound))
      saveCreditsCount c' >>= \case
        Left err -> logErr "credits" "ERROR" (show err)
        Right _ -> return ()

coinAgent :: System -> IO a
coinAgent sys = do
  -- eavesdrop on the input stream
  ctrlIn' <- dupChan (sysCtrlIn sys)
  let creditsVar = sysCreditsVar sys
  forever $ do
    i <- readChan ctrlIn'
    case i of
      InsertCoin -> atomically (modifyTVar creditsVar (+1))
      _ -> return ()

sceneThread :: System -> VideoOut -> Fix IO -> IO a
sceneThread sys vout (Fix begin) = loop begin where
  loop runScene = withAsync runScene $ \scene -> do
    result <- waitCatch scene
    case result of
      Left err -> do
        let msg = show err
        let klass = head (words msg)
        logErr "scene" "ERROR" msg
        bg <- readIORef (fst vout)
        writeVideo vout $
          bg <>
          Block (V4 0 0 320 40) (V3 255 0 0) <>
          Block (V4 3 3 314 34) (V3 0 0 0) <>
          (Shift (V2 8 10) (gfxText "SOFTWARE FAILURE. PRESS (1) TO CONTINUE.")) <>
          (Shift (V2 8 24) (gfxText ("GURU MEDITATION #0000." ++ klass)))
        loop (errorScene sys msg)
      Right (Fix next) -> do
        writeVideo vout Blank
        performMajorGC
        loop next

errorScene :: System -> String -> IO (Fix IO)
errorScene sys msg = do
  hang
