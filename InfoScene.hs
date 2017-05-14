module InfoScene where

import Types
import System
import Util

import {-# SOURCE #-} TitleScene (titleScene)

infoScene :: System -> Fix IO
infoScene sys = Fix $ do
  hang
  return (titleScene sys)

