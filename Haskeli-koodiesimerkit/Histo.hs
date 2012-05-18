module Main where

import qualified Data.Map as M

histo [] = M.empty
histo (c:cs) = M.insertWith (+) c 1 $ histo cs

main = do c <- getContents
          print (histo c)
