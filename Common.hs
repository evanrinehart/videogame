module Common where

import Types

screenW, screenH :: Int
(screenW,screenH) = (320,240)

sampleRate :: Integer
sampleRate = 44100

ms :: Integer -> Delta
ms i = i * 1000000

us :: Integer -> Delta
us i = (i * sampleRate) `div` 1000

