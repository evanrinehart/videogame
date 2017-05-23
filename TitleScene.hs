module TitleScene where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad
import Control.Exception
import Data.Monoid

import Linear

import Types
import HighScore
import Graphics
import Animation
import Audio
import System
import Credits
import Util
import Null
import Debug
import Error

import InfoScene
import Splash

titleScene :: System -> Fix IO
titleScene sys@(Sys videoOut ctrlIn _ creditsVar _ _) = Fix $ do
  animVar   <- newTVarIO titleAnimation
  ender     <- async (endOfSceneAgent sys animVar)
  animator  <- async (animatorAgent animVar)
  render    <- async (renderAgent videoOut animVar creditsVar)
  input     <- async (inputAgent sys ctrlIn creditsVar)
  (_, next) <- waitAnyCancel [ender,animator,render,input]
  return next

renderAgent :: VideoOut -> TVar (Animation Picture) -> TVar Integer -> IO a
renderAgent videoOut animVar creditsVar = forever $ do
  pic <- atomically $ do
    title <- view <$> readTVar animVar
    cr    <- gfxText . creditsReport <$> readTVar creditsVar
    return (title <> (Shift (V2 220 232) cr))
  writeVideo videoOut pic

endOfSceneAgent :: System -> TVar (Animation Picture) -> IO (Fix IO)
endOfSceneAgent sys animVar = atomically $ do
  anim <- readTVar animVar
  check (isExpired anim)
  return (infoScene sys)

inputAgent :: System -> Chan Input -> TVar Integer -> IO (Fix IO)
inputAgent sys ctrlIn creditsVar = loop where
  loop = do
    (ctrl,cr) <- (,) <$> readChan ctrlIn <*> atomically (readTVar creditsVar)
    case ctrl of
      StartButton playerNo -> startGameAs playerNo cr
      JumpButton _ Press -> do
        b <- sysCommitScore sys (ValidEntry "ABC" 25000)
        logMsg "scene:title" (show b)
        --return (nullScene sys) --(infoScene sys)
        loop
      Joystick _ _ _ -> throwIO (videoGameError "fruit cake")
      _ -> loop
  startGameAs playerNo cr
    | cr < 1 = return (nullScene sys)--(infoScene sys)
    | cr > 0 = do -- redeem credits, start game
        atomically (modifyTVar' creditsVar (subtract 1))
        --return (gameplayScene sys)
        return (splashScene sys)

animatorAgent :: TVar (Animation Picture) -> IO a
animatorAgent animVar = hang

titleAnimation :: Animation Picture
titleAnimation = pure Blank

