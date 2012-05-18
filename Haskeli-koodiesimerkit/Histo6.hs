{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Map as M
import System.IO

go :: M.Map Char Int -> IO ()
go !m = do e <- isEOF
           if e 
             then print m
             else do c <- getChar
                     go (M.insertWith' (+) c 1 m)

main = go M.empty
