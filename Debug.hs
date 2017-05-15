{-# LANGUAGE BangPatterns #-}
module Debug where

import System.IO
import System.Clock
import Data.Time
import Control.Concurrent.MVar
import Control.Concurrent.STM
import System.IO.Unsafe
import Control.Exception
import Control.Monad
import Data.Typeable

fetchTime :: IO (UTCTime, Integer)
fetchTime = do
  !utc <- getCurrentTime
  !nanos <- fmap toNanoSecs (getTime Monotonic)
  return (utc, nanos)

newtype IVar a = IVar (MVar a)
readIVar :: IVar a -> a
readIVar (IVar mv) = unsafePerformIO (readMVar mv)

data IVarError = IVarError deriving Typeable

instance Exception IVarError

instance Show IVarError where
  show _ = "writeIVar executed twice on the same ivar"

newEmptyIVar :: IO (IVar a)
newEmptyIVar = fmap IVar newEmptyMVar

writeIVar :: IVar a -> a -> IO ()
writeIVar (IVar mv) x = do
  ok <- tryPutMVar mv x
  when (not ok) (throwIO IVarError)

programStartTime :: IVar (UTCTime, Integer)
programStartTime = unsafePerformIO newEmptyIVar

globalLogQueue :: TBQueue LogEntry
globalLogQueue = unsafePerformIO (newTBQueueIO 100)

initializeGlobalProgramStartTime :: IO ()
initializeGlobalProgramStartTime = do
  time <- fetchTime
  writeIVar programStartTime time

formatTimestamp :: (UTCTime, Integer) -> String
formatTimestamp (utc, t1) =
  let (_,t0) = readIVar programStartTime in
  let s1 = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" utc in
  let delta = t1 - t0 in
  let (totalSecs,nanos) = divMod delta 1000000000 in
  let (totalMins, secs) = divMod totalSecs 60 in
  let (totalHours, mins) = divMod totalMins 60 in
  let (days, hours) = divMod totalHours 24 in
  let pad n s = (replicate (n - length s) '0') ++ s in
  s1 ++ " (day " ++ show days ++ " " ++ 
  pad 2 (show hours) ++ ":" ++ pad 2 (show mins) ++ ":" ++
  pad 2 (show secs) ++ "." ++ pad 9 (show nanos) ++ ")"

fixedCol :: Int -> String -> String
fixedCol w txt =
  let right w = reverse . take w . reverse in
  let txt' = right w txt in
  (replicate (w - length txt') ' ') ++ txt'

whoW = 17

getCurrentTimestamp :: IO String
getCurrentTimestamp = fmap formatTimestamp fetchTime

data LogEntry =
  NormalLog String String |
  ErrorLog String String String
    deriving Show

loggerAgent :: IO a
loggerAgent = do
  stamp <- fmap formatTimestamp fetchTime
  putStrLn (stamp ++ " " ++ fixedCol whoW "o_O" ++ " >=====< Video Game (C) Evan R >=====<")
  forever $ do
    entry <- atomically (readTBQueue globalLogQueue)
    case entry of
      NormalLog who msg -> primLog who msg
      ErrorLog who kind msg -> primLogErr who kind msg

primLog :: String -> String -> IO ()
primLog who msg = do
  stamp <- fmap formatTimestamp fetchTime
  putStrLn (stamp ++ " " ++ fixedCol whoW who ++ " " ++ msg)

primLogErr :: String -> String -> String -> IO ()
primLogErr who kind msg = do
  stamp <- fmap formatTimestamp fetchTime
  hPutStrLn stderr (stamp ++ " " ++ fixedCol whoW who ++ " *" ++ kind ++ "* " ++ msg)

logMsg :: String -> String -> IO ()
logMsg who msg = do
  atomically (writeTBQueue globalLogQueue (NormalLog who msg))

logErr :: String -> String -> String -> IO ()
logErr who kind msg = do
  atomically (writeTBQueue globalLogQueue (ErrorLog who kind msg))
