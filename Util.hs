{-# LANGUAGE TupleSections #-}
module Util where

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception
import Control.Monad
import LongSleep

import System.Random

import Numeric.Natural

import Error

watchForChanges :: Eq a => a -> TVar a -> (a -> a -> IO ()) -> IO b
watchForChanges x0 tv action = go x0 where
  go x = do
    x' <- atomically $ do
      x' <- readTVar tv
      when (x' == x) retry
      return x'
    action x x'
    go x'

hang = do
  threadDelay 1000000
  hang

pollAny :: [Async a] -> IO (Maybe (Either SomeException a))
pollAny [] = return Nothing
pollAny (a:as) = do
  result <- poll a
  case result of
    Nothing -> pollAny as
    Just x -> return (Just x)

races :: [IO a] -> IO a
races [io] = io
races (io:ios) = do
  r <- race io (races ios)
  case r of
    Left x  -> return x
    Right x -> return x

spawnGroup :: [(String,IO a)] -> IO (String, SomeException)
spawnGroup cs = races racers where
  racers = map f cs
  f (name, io) = do
    withAsync io $ \a -> do
      r <- waitCatch a
      case r of
        Left err -> return (name, err)
        Right x  -> return (name, toException $ bug "spawnGroup" (name ++ " ended normally."))

-- try the action 1 time.
-- if it fails, retry maxRetries times.
limitedRetrier :: (String -> IO ()) -> Natural -> IO a -> IO a
limitedRetrier log maxRetries action = go maxRetries where
  go n = do
    next <- withAsync action $ \a -> do
      r <- waitCatch a
      case r of
        Right x -> return (return x)
        Left err -> do
          log (show err ++ " ... " ++ show n ++ " tries left.")
          if n == 0
          then do
            log "Game Over Man"
            return $ throwIO err
          else do
            return $ go (n - 1)
    next

-- try the action once.
-- if it fails, retry in 1sec, then 2sec, then 4sec, then 8sec, and so on
unlimitedExponentialRetrier :: (String -> IO ()) -> IO a -> IO a
unlimitedExponentialRetrier alert action = go 1000000 where
  go delay = do
    next <- withAsync action $ \a -> do
      r <- waitCatch a
      case r of
        Right x -> return (return x)
        Left err -> do
          alert (show err ++ " ... retrying in " ++ formatDuration delay)
          longSleep delay
          let delay' = min (6 * 3600 * 1000000) (2*delay)
          return (go delay')
    next

plural :: Integer -> String -> String
plural 0 word = word ++ "s"
plural 1 word = word
plural n word = word ++ "s"

formatDuration :: Integer -> String
formatDuration micros =
  let totalSecs = micros `div` 1000000 in
  let (totalMins, secs) = divMod totalSecs 60 in
  let (hours, mins) = divMod totalMins 60 in
  if hours > 0 then show (fromInteger hours) ++ " " ++ plural hours "hour"
  else if mins > 0 then show (fromInteger mins) ++ " " ++ plural mins "minute"
  else show secs ++ " " ++ plural secs "second"

test :: IO Char
test = do
  x <- randomIO :: IO Double
  if x < 0.9 then throwIO (userError "fruit") else return 'j'

pollThrow :: Async a -> IO (Maybe a)
pollThrow a = do
  r <- poll a
  case r of
    Nothing -> return Nothing
    Just (Left err) -> throwIO err
    Just (Right x) -> return (Just x)
