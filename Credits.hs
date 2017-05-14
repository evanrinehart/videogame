module Credits where

import Data.IORef
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import Network

import HighScore
import Util

creditsReport :: Integer -> String
creditsReport cr
  | cr < 0    = "CREDITS ERR"
  | cr < 100  = "CREDITS " ++ show cr
  | otherwise = "CREDITS 99+"

subtractCredits :: TVar Integer -> Integer -> IO ()
subtractCredits tv amount = do
  result <- atomically $ do
    c <- readTVar tv
    if amount > c
      then return (Left c)
      else do
        let c' = c - amount
        writeTVar tv c'
        return (Right c')
  case result of
    Left c -> hPutStrLn stderr $
      "subtractCredits bug: taking " ++ show amount ++ " credits from " ++ show c
    Right _ -> return ()

saveCreditsCount :: Integer -> IO ()
saveCreditsCount c = withCreditsDemon () $ \h -> do
  hPutStrLn h (show c)

fetchCreditsCount :: IO Integer
fetchCreditsCount = withCreditsDemon 0 $ \h -> do
  hPutStrLn h ""
  raw <- hGetLine h
  case parseNumber raw of
    Right (n,"") -> return n
    Left _ -> do
      hPutStrLn stderr ("bad result from credits demon " ++ show raw)
      return 0

connectToCreditsDemon :: IO Handle
connectToCreditsDemon = connectTo "localhost" (PortNumber 9001)
  
withCreditsDemon :: a -> (Handle -> IO a) -> IO a
withCreditsDemon fallback action = do
  hmm <- try $ bracket connectToCreditsDemon hClose action
  case hmm of
    Left e -> do
      hPutStrLn stderr ("credits demon: "++show (e :: SomeException))
      return fallback
    Right x -> return x
