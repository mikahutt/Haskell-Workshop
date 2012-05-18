{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Map as M

histo s = go M.empty s
  where go :: M.Map Char Int -> [Char] -> M.Map Char Int
        go !m (c:cs) = go (M.insertWith' (+) c 1 m) cs
        go !m [] = m

main = do c <- getContents
          print (histo c)
