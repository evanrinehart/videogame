module Output where

import System.IO

import Types
import HighScore
import Credits

execOutput :: HighScoreCache -> CreditsCounter -> Output -> IO ()
execOutput hsCache counter out = do
  print out
  case out of
    CommitHighScore name score -> case validateEntry name score of
      Nothing -> hPutStrLn stderr ("can't commit invalid high score entry " ++ name ++ " " ++ show score)
      Just entry -> registrateHighScore hsCache entry
    AddCredits c -> addCredits counter c
    SpendCredits c -> delCredits counter c
    PlaySound i -> return ()
