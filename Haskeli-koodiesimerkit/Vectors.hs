module Main where

import Data.List

data Vector = Vector !Double !Double !Double
  deriving Show

(|+|) :: Vector -> Vector -> Vector
Vector a b c |+| Vector d e f = Vector (a+d) (b+e) (c+f)

sumVectors :: [Vector] -> Vector
sumVectors vs = foldl' (|+|) (Vector 0 0 0) vs

mk i = Vector i (2*i) (sqrt i)

main =
  print $ sumVectors $ map mk [1..100000]