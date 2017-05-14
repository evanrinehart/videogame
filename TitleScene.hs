module TitleScene where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad
import Data.Monoid

import Types
import HighScore
import Graphics
import Animation
import Audio
import System
import Credits
import Util

import InfoScene

titleScene :: System -> Fix IO
titleScene sys@(Sys videoOut ctrlIn _ creditsVar _ _) = Fix $ do
  animVar   <- newTVarIO titleAnimation
  ender     <- async (endOfSceneAgent sys animVar)
  animator  <- async (animatorAgent animVar)
  render    <- async (renderAgent videoOut animVar creditsVar)
  input     <- async (inputAgent sys ctrlIn creditsVar)
  (_, next) <- waitAnyCancel [ender,animator,render,input]
  return next

renderAgent :: TMVar Picture -> TVar (Animation Picture) -> TVar Integer -> IO a
renderAgent videoOut animVar creditsVar = forever $ atomically $ do
  title <- view <$> readTVar animVar
  cr    <- gfxText . creditsReport <$> readTVar creditsVar
  putTMVar videoOut (title <> cr)

endOfSceneAgent :: System -> TVar (Animation Picture) -> IO (Fix IO)
endOfSceneAgent sys animVar = atomically $ do
  anim <- readTVar animVar
  check (isExpired anim)
  return (infoScene sys)

inputAgent :: System -> TChan Input -> TVar Integer -> IO (Fix IO)
inputAgent sys ctrlIn creditsVar = loop where
  loop = do
    (ctrl,cr) <- atomically $ (,) <$> readTChan ctrlIn <*> readTVar creditsVar
    case ctrl of
      StartButton playerNo -> startGameAs playerNo cr
      JumpButton _ Press -> return (infoScene sys)
      _ -> loop
  startGameAs playerNo cr
    | cr < 1 = return (infoScene sys)
    | cr > 0 = do -- redeem credits, start game
        atomically (modifyTVar' creditsVar (subtract 1))
        return (gameplayScene sys)

animatorAgent :: TVar (Animation Picture) -> IO a
animatorAgent animVar = hang
  
gameplayScene :: System -> Fix IO
gameplayScene sys = Fix hang

titleAnimation :: Animation Picture
titleAnimation = pure Blank

