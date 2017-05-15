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
import Debug
import Error

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
    Left c -> logErr "main:credits" "BUG" $
      "subtractCredits taking " ++ show amount ++ " credits from " ++ show c
    Right _ -> return ()

saveCreditsCount :: Integer -> IO (Either SomeException ())
saveCreditsCount c = withCreditsDemon $ \h -> hPutStrLn h (show c)

fetchCreditsCount :: IO (Either SomeException Integer)
fetchCreditsCount = withCreditsDemon $ \h -> do
  hPutStrLn h ""
  raw <- hGetLine h
  case parseNumber raw of
    Right (n,"") -> return n
    Left err -> throwIO (badFormatError err)

connectToCreditsDemon :: IO Handle
connectToCreditsDemon = connectTo "localhost" (PortNumber 9001)
  
withCreditsDemon :: (Handle -> IO a) -> IO (Either SomeException a)
withCreditsDemon action = try $ bracket connectToCreditsDemon hClose action
