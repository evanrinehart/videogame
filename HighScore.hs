{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module HighScore where

import Types
import Data.IORef
import System.Directory
import System.IO
import Data.Char
import Control.Monad
import Control.Concurrent
import Data.Time

import Debug

type HighScores = [(String,Integer)]
type CommitChan = MVar (ValidEntry, MVar CommitStatus)

data CommitStatus = CommitOK | CommitIncomplete deriving Show

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz-_?!0123456789"

initialHighScores :: [(String, Integer)]
initialHighScores =
  [("BIF", 50000)
  ,("BEJ", 40000)
  ,("NER", 30000)
  ,("ENE", 20000)
  ,("VIN", 10000)]

data ValidEntry = ValidEntry String Integer deriving Show

validateEntry :: String -> Integer -> Maybe ValidEntry
validateEntry name score = let lname = map toLower name in
  if | length name /= 3                  -> Nothing
     | not (all (`elem` alphabet) lname) -> Nothing
     | score < 0                         -> Nothing
     | otherwise                         -> Just (ValidEntry lname score)

tempName = "high_scores.temp"
backupName = "high_scores.bak"
finalName = "high_scores"

fetchHighScores :: FilePath -> IO HighScores
fetchHighScores dirpath = do
  let tempPath = dirpath ++ "/" ++ tempName
  let backupPath = dirpath ++ "/" ++ backupName
  let finalPath = dirpath ++ "/" ++ finalName
  touchFile backupPath
  touchFile finalPath
  fmap parseHighScores (readFile finalPath) >>= \case
    Right hs -> return hs
    Left msg -> do
      logErr "main" "ERROR" ("parse high score file (1st try): " ++ msg)
      timestamp <- formatTime defaultTimeLocale "%Y-%M-%dT%H:%M:%S" <$> getCurrentTime
      let forensics = dirpath ++ "/high_scores.corrupted." ++ timestamp
      renameFile finalPath forensics
      renameFile backupPath finalPath
      fmap parseHighScores (readFile finalPath) >>= \case
        Right hs -> return hs
        Left msg -> do
          logErr "main" "ERROR" ("parse high score file (2nd try): " ++ msg)
          let forensics = dirpath ++ "/high_scores.corrupted.2." ++ timestamp
          renameFile finalPath forensics
          writeFile tempPath (unparseHighScores initialHighScores)
          renameFile tempPath finalPath
          return initialHighScores

updateHighScores :: ValidEntry -> HighScores -> HighScores
updateHighScores (ValidEntry name score) hs =
  take 5 (insertEntry (map toUpper name) score hs)

saveScores :: FilePath -> HighScores -> IO ()
saveScores dirpath scores = do
  let tempPath = dirpath ++ "/" ++ tempName
  let backupPath = dirpath ++ "/" ++ backupName
  let finalPath = dirpath ++ "/" ++ finalName
  writeFile tempPath (unparseHighScores scores)
  renameFile finalPath backupPath
  renameFile tempPath finalPath

insertEntry :: String -> Integer -> HighScores -> HighScores
insertEntry name score [] = [(name,score)]
insertEntry name score ((x,y):rest)
  | score > y = ((name,score) : (x,y) : rest)
  | otherwise = (x,y) : insertEntry name score rest

touchFile :: FilePath -> IO ()
touchFile path = do
  b <- doesFileExist path
  when (not b) (writeFile path (unparseHighScores initialHighScores))

type Parser a = String -> Either String (a,String)

parseHighScores :: String -> Either String HighScores
parseHighScores s = do
  (line1, s) <- parseEntry s
  (line2, s) <- parseEntry s
  (line3, s) <- parseEntry s
  (line4, s) <- parseEntry s
  (line5, s) <- parseEntry s
  case s of
    "" -> return [line1,line2,line3,line4,line5]
    other -> Left "expected end of input"

parseEntry :: Parser (String,Integer)
parseEntry s = do
  (letter1, s) <- parseLetter s
  (letter2, s) <- parseLetter s
  (letter3, s) <- parseLetter s
  (_, s) <- parseSpace s
  (score, s) <- parseNumber s
  (_, s) <- parseNewline s
  return (([letter1, letter2, letter3], score), s)

parseLetter :: Parser Char
parseLetter "" = Left "unexpected end of input"
parseLetter (c:cs)
  | toLower c `elem` alphabet = Right (c, cs)
  | otherwise = Left ("unexpected "++show c++ " expected letter")

parseNumber :: Parser Integer
parseNumber s = case reads s :: [(Integer,String)] of
  [(i,rest)] | i < 0 -> Left ("unexpected negative number " ++ show i)
             | otherwise -> Right (i, rest)
  [] -> Left "expected number"
  other -> Left ("ambiguous parse " ++ show (map fst other))

parseSpace :: Parser ()
parseSpace "" = Left "unexpected end of input"
parseSpace (' ':cs) = Right ((), cs)
parseSpace (c:_) = Left ("unexpected " ++ show c ++ " expected space")

parseNewline :: Parser ()
parseNewline "" = Left "unexpected end of input"
parseNewline ('\n':cs) = Right ((), cs)
parseNewline (c:_) = Left ("unexpected " ++ show c ++ " expected newline")

unparseHighScores :: HighScores -> String
unparseHighScores [] = ""
unparseHighScores ((name,score):next) =
  (name ++ " " ++ show score ++ "\n") ++ unparseHighScores next


