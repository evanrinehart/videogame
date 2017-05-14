{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (when, forever, forM_)  
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception
import System.Exit (exitSuccess, exitFailure)
import Data.IORef
import Foreign.C.Types
import System.IO
import Data.List
import Data.Maybe
import System.Mem

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

sampleRate = 44100
audioBufferSize = 2048
(screenW, screenH) = (320, 240)

main :: IO ()
main = do
  (sdlRenderer, sprites, sys) <- setup
  let videoOut = sysVideoOut sys
  let ctrlIn = sysCtrlIn sys
  let initialScene = nullScene sys
  putStrLn ".oO ..."
  let spawn msg begin = putStrLn msg >> async begin
  putStrLn ".oO Spawning credits persistence agent"
  putStrLn ".oO Spawning coin slot servant"
  putStrLn ".oO Spawning high score archival agent"
  putStrLn ".oO Spawning the scene"
  threads <- spawnGroup "main"
    [("credits-persist", creditsAgent sys)
    ,("coin-slot", coinAgent sys)
    ,("highscore-saver", highScoreAgent sys "database")
    ,("scene", sceneThread videoOut initialScene)]
  putStrLn ".oO Entering the main loop. Good luck!"
  forever $ do
    -- get events if any
    evs <- pollEvents
    let inputs = extractInputs evs
    -- push input events
    atomically $ forM_ inputs (writeTChan ctrlIn)
    -- if should exit, exit
    when (shouldQuit evs) $ do
      putStrLn ".oO Got the signal to exit."
      putStrLn "*** *** ***"
      exitSuccess
    -- get a picture
    picture <- atomically (takeTMVar videoOut)
    -- display the picture
    rendererDrawColor sdlRenderer $= V4 0 0 0 255
    clear sdlRenderer
    renderPicture sdlRenderer sprites picture
    -- block until displayed
    present sdlRenderer 
    -- see if any main threads crashes
    status <- poll threads
    case status of
      Just (Right (who, err)) -> throwIO (videoGameError who err)
      Just (Left err) -> throwIO (videoGameError "main" err)
      Nothing -> return ()

sceneThread :: VideoOut -> Fix IO -> IO a
sceneThread vout (Fix begin) = loop begin where
  loop runScene = withAsync runScene $ \scene -> do
    result <- waitCatch scene
    case result of
      Left err -> do
        --hPutStrLn stderr (show err)
        -- ... show the error scene
        throwIO err
      Right (Fix next) -> do
        atomically $ putTMVar vout Blank -- blank the video
        performMajorGC
        --withAsync (blanker vout) $ do
        loop next
        
blanker :: VideoOut -> IO a
blanker out = forever $ atomically $ putTMVar out Blank

devNull :: ControlIn -> IO a
devNull inCh = forever $ do
  i <- atomically (readTChan inCh)
  print i

nullScene :: System -> Fix IO
nullScene sys =
  let vout = sysVideoOut sys in
  let inCh = sysCtrlIn sys in
  Fix $
    withAsync (blanker vout) $ \_ ->
    withAsync (devNull inCh) $ \_ -> do
      putStrLn "null scene"
      hang

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
shouldQuit = isJust . find isQuit

setup :: IO (Renderer, Texture, System)
setup = do
  putStrLn "<|> Video Game (C) Evan R <|>"
  putStrLn ".oO Begin boot-up sequence"
  -- "global" STM vars and chans
  videoOut <- newEmptyTMVarIO
  ctrlIn <- newTChanIO
  soundCtrl <- newTChanIO
  commitCh <- newEmptyTMVarIO
  cr0 <- fetchCreditsCount
  putStr ".oO Restoring credits... "
  putStrLn ("There are " ++ show cr0 ++ " credits in the machine.")
  creditsVar <- newTVarIO cr0
  let dbpath = "database"
  scores <- fetchHighScores dbpath
  putStr ".oO Recovering high scores... "
  putStrLn (show scores)
  highScoresVar <- newTVarIO scores
  let sys = Sys videoOut ctrlIn soundCtrl creditsVar highScoresVar commitCh
  -- sdl
  putStr ".oO Initializing SDL... "
  initializeAll
  putStrLn "CHECK."
  -- video
  putStr ".oO Creating window... "
  window <- createWindow
    "VIDEO GAME"
    (defaultWindow
      { windowInitialSize=V2 (fromIntegral screenW) (fromIntegral screenH)
      , windowMode=Windowed
      })
  putStrLn "CHECK."
  putStr ".oO Creating accelerated renderer... "
  renderer <- createRenderer
    window
    (-1)
    (defaultRenderer {rendererType=AcceleratedVSyncRenderer})
  putStrLn "CHECK."
  putStr ".oO Loading sprites... "
  spriteSurf <- loadBMP "sprites.bmp"
  putStr "CHECK... "
  sprites <- createTextureFromSurface renderer spriteSurf
  freeSurface spriteSurf
  putStrLn "CHECK."
  -- audio
  putStr $ 
    ".oO Opening audio device {sampleRate = " ++ show sampleRate ++
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
  putStr "CHECK... "
  setAudioDevicePlaybackState device Play
  putStrLn "CHECK."
  -- initial scene
  return (renderer, sprites, sys)

creditsAgent :: System -> IO a
creditsAgent (Sys _ _ soundCtrl creditsVar _ _) = do
  c0 <- atomically (readTVar creditsVar)
  watchForChanges c0 creditsVar f where
    f c c' | c' > c    = do
               atomically (writeTChan soundCtrl (PlaySound CoinSound))
               saveCreditsCount c'
           | otherwise = return ()

coinAgent :: System -> IO a
coinAgent sys = do
  -- eavesdrop on the input stream
  ctrlIn' <- atomically (dupTChan (sysCtrlIn sys))
  let creditsVar = sysCreditsVar sys
  forever $ atomically $ do
    i <- readTChan ctrlIn'
    case i of
      InsertCoin -> modifyTVar creditsVar (+1)
      _ -> return ()

highScoreAgent :: System -> FilePath -> IO a
highScoreAgent sys dirname = do
  let hsVar = sysHighScoresVar sys
  let commitCh = sysCommitScore sys
  let soundCtrl = sysSoundCtrl sys
  forever $ do
    entry <- atomically (takeTMVar commitCh)
    commitHighScore dirname hsVar entry
    atomically (writeTChan soundCtrl (PlaySound RegistrateSound))

