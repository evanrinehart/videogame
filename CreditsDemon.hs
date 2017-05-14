module Main where

import Data.IORef
import System.IO
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char

import Network

main = do
  ref <- newIORef 0
  server <- listenOn (PortNumber 9001)
  forever $ do
    (h, peer, port) <- accept server
    when (peer == "localhost") $ do
      flip forkFinally (\_ -> hClose h) $ do
        l <- hGetLine h
        if null (filter (not . isSpace) l)
          then fmap show (readIORef ref) >>= hPutStrLn h
          else case reads l :: [(Integer,String)] of
            [(i,_)] | i < 0 -> return ()
                     | otherwise -> writeIORef ref i
            _ -> return ()
      return ()
