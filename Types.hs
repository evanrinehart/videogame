{-# LANGUAGE DeriveFunctor #-}
module Types where

import Linear

import Data.HashMap.Strict (HashMap)
import Data.Word
import Control.Concurrent
import Control.Exception
import Control.DeepSeq

newtype Fix f = Fix { unFix :: f (Fix f) }
type Lens a b = (b -> b) -> a -> a

data DirWE    = East' | West' deriving Show
data PlayerNo = Player1 | Player2 deriving Show
data DirNSWE  = North | South | East | West deriving Show
data SwitchAction = Press | Release deriving Show
data Input =
  InsertCoin |
  StartButton !PlayerNo |
  JumpButton !PlayerNo !SwitchAction |
  Joystick !PlayerNo !DirNSWE !SwitchAction
    deriving Show

type Color = V3 Word8
type Delta = Integer
type Speed = Rational
type SampleRate = Integer

