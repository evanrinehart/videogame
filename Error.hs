module Error where

import Data.Typeable
import Control.Exception

data VideoGameException = VideoGameException String
  deriving Typeable

instance Show VideoGameException where
  show (VideoGameException msg) =
    "VideoGameError " ++ msg

instance Exception VideoGameException

videoGameError :: String -> VideoGameException
videoGameError = VideoGameException

data Bug = Bug String String 
  deriving Typeable

instance Show Bug where
  show (Bug info msg) = "Bug (" ++ info ++ ") (" ++ msg ++ ")"

instance Exception Bug

bug :: String -> String -> Bug
bug = Bug


data BadFormatException = BadFormatException String
  deriving (Show, Typeable)

instance Exception BadFormatException

badFormatError = BadFormatException
