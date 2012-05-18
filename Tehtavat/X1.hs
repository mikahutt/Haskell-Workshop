-- Palauta kaikki vastauksesi tässä tiedostossa JFO:n palautusjärjestelmään:
-- http://joamaki.users.cs.helsinki.fi/jfo/
--
-- Ei testejä tällä kertaa, voit koodata omasi :)

module X1 where

-- Tehtävä 1: Kirjoita auki miten seuraavat lausekkeet evaluoituvat
-- arvoiksi. Kirjoita kaikki välivaiheet. Tässä vielä määritelmiä:

{-
const :: a -> b -> b
const _ y = y

length [] = 0
length (x:xs) = 1 + length xs

takeWhile p [] = []
takeWhile p (x:xs)
  | p x = x : takeWhile p xs
  | otherwise = []

map f [] = []
map f (x:xs) = f x : map f xs

not True = False
not False = True
                
[] ++ ys = ys
(x:xs) ++ ys = x:(xs++ys)
-}

-- a) not (const (7+8+9) (1+1==2))

-- d) length (takeWhile (<9) (map (^2) [0..]))

-- c) not (seq (7+8+9) (1+1==2))

-- d) let x = [1,2,3]; y = x++y in take 5 y

-- Tehtävä 2: Piirrä seuraavia lausekkeita vastaavat graafit

-- a) (1+1)*(1+1)
-- b) let x = 1+1
--        y = x+x
--    in y - (x+x)
-- c) let num = 7
--        nums = num:nums
--        ls = take num nums
--    in (num+1):ls

-- Tehtävä 3: Toteuta funktio reverse' joka kääntää listan. Tee
-- funktiostasi sellainen, että paluuarvon täysin evaluoiminen vaatii
-- lineaarisen ajan. Perustele ajankäyttö myös!

reverse' :: [a] -> [a]
reverse' xs = undefined

-- Tehtävä 4: Kirjoita vakiotilassa toimiva funktio, joka hakee
-- merkkijonosta juuri ennen ensimmäistä välilyöntiä ennen esiintyvän
-- vokaalin.
--
-- Perustele miksi toteutuksesi toimii vakiotilassa!
-- 
-- Esimerkkejä:
--   findIt "axxxxxxxxxxxxxxxxxxx " ==> 'a'
--   findIt "herpaderp hyi hai!" ==> 'e'

findIt :: String -> Char
findIt s = undefined

-- Tehtävä 5: Toteuta vakiotilassa toimiva funktio count, joka laskee
-- montako predikaatin täyttävää alkiota listasta löytyy. Perustele
-- miksi toteutuksesi on vakiotilainen.
--
-- Huom! Toteutuksen tulee olla vakiotilainen ilman optimointeja.

count :: (a -> Bool) -> [a] -> [a]
count p xs = undefined

-- Tehtävä 6: Seuraavassa fibonaccin lukuja laskevassa funktiossa on
-- muistivuoto (koita vaikkapa laskea 600000. fibonaccin luku). Korjaa
-- se.
--
-- PS. Voit kopioida funktion ja alla olevan mainin omaan tiedostoonsa
-- ja kääntää sen: näin voit profiloida sitä.

fib :: Integer -> Integer
fib n = go 0 1 n
  where go a b 0 = a
        go a b n = go b (a+b) (n-1)

{-
main = do
  [k] <- getArgs
  let n = read k
  print (fib n)
-}

-- Tehtävä 7: Toteuta funktio joka laskee listan keskiarvon
-- vuotamatta muistia.
-- 
-- PS. Alla jälleen main avuksesi. Tee toteutuksestasi muistiprofiili
-- jotta näet että muistinkäyttö on tarpeeksi pieni.

avg :: [Double] -> Double
avg ds = undefined

{-
main = do
  [k] <- getArgs
  let n = read k
  print (avg [1..n])
-}

-- Tehtävä 8: Mitä eroa on seuraavien kahden funktion
-- suorituskyvyllä? Kirjoita auki muutama funktioitten suoritus jos et
-- keksi suoraan.

countZeros0 :: [Int] -> Int
countZeros0 xs = go 0 xs
  where go k [] = k
        go k (0:xs) = go (k+1) xs
        go k (_:xs) = go k xs
        
countZeros1 :: [Int] -> Int
countZeros1 xs = go 0 xs
  where go k [] = k
        go k (x:xs) = go (if x == 0 then k+1 else k)  xs
        
-- Tehtävä 9&10: Tuota aika-, ja muistiprofiilit seuraavasta ohjelmasta.
-- Mikä on vialla? Korjaa ohjelma.
--
-- Yksi piste ongelman tunnistamisesta, toinen korjaamisesta.        
--
-- PS. käännä tiedosto ilman optimointeja (-O0)

{-
module Main where

import System.Random
import System.Environment
import Control.Monad

data Tree = Leaf | Node Int Tree Tree
  deriving Show

ins Leaf x = Node x Leaf Leaf
ins (Node y l r) x
  | x > y = Node y l (ins r x)
  | x < y = Node y (ins l x) r
  | otherwise = Node y l r

siz Leaf = 0
siz (Node _ l r) = 1 + siz l + siz r

main = do
  [k] <- getArgs
  let n = read k
      g = mkStdGen 1
      dat = take n $ randoms g
  print $ siz $ foldl ins Leaf dat
-}        
        
-- Tehtävä 10: Tee funktio analyze, joka laskee merkkijonossa
-- esiintyvien välilyöntien määrän ja merkkijonon pituuden.
--
-- Toteuta funktiosi foldr':n avulla ja käytä ahkeraa tietotyyppiä
-- välitulosten tallentamiseen. Varmista että toteutuksesi ei vuoda
-- muistia (esim. allaolevan mainin avulla).
--
-- Esimerkkejä:
--   analyze "Hello world!" ==> (12,1)
--   analyze "   " ==> (3,3)        

analyze :: String -> (Int,Int)
analyze = undefined

{-
main = do
  [k] <- getArgs
  let n = read k 
  print $ analyze $ take n $ cycle "Hello world! "
-}

