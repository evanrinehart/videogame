module Error where

import Data.Typeable
import Control.Exception

data VideoGameException = VideoGameException String SomeException
  deriving Typeable

instance Show VideoGameException where
  show (VideoGameException who e) =
    "GURU MEDITATION in " ++ who ++ " (" ++ show e ++ ")"

instance Exception VideoGameException

videoGameError :: Exception e => String -> e -> VideoGameException
videoGameError who = VideoGameException who . toException

data Bug = Bug String String 
  deriving Typeable

instance Show Bug where
  show (Bug info msg) = "bug (" ++ info ++ ") (" ++ msg ++ ")"

instance Exception Bug

bug :: String -> String -> Bug
bug = Bug
