module Main where

import qualified Data.Vector.Unboxed as V
--import qualified Data.ByteString.Lazy as B
import Data.List

initial :: V.Vector Int
initial = V.replicate 256 0

histo s = V.accum (+) initial (map (\x -> (fromEnum x,1)) s)

main = do c <- getContents
          print (histo c)
