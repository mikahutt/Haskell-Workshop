module Main where

length''                  :: [a] -> Int
length'' l                =  len l 0
  where
    len :: [a] -> Int -> Int
    len []     a = a
    len (_:xs) a = len xs (a+1)
    
main = print (length'' [1..10^6])
