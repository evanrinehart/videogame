module Output where

import Types

execOutput :: Output -> IO ()
execOutput out = print out
