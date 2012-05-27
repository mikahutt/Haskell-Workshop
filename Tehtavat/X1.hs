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
{-
-- a) not (const (7+8+9) (1+1==2))

=> not (const (7+8+9) (2 == 2))
=> not (const (7+8+9) True)
=> not True
=> False

-- b) length (takeWhile (<9) (map (^2) [0..]))

=> length (takeWhile (<9) (map (^2) 0: [1..]))
=> length (takeWhile (<9) (0^2: map (^2) [1..]))
=> length (takeWhile (<9) (0: map (^2) [1..]))
=> length (0: takeWhile (<9) (map (^2) [1..]))
=> 1 + length (takeWhile (<9) (map (^2) [1..]))
=> 1 + length (takeWhile (<9) (map (^2) 1: [2..]))
=> 1 + length (takeWhile (<9) (1^2: map (^2) [2..]))
=> 1 + length (takeWhile (<9) (1: map (^2) [2..]))
=> 1 + length (1: takeWhile (<9) (map (^2) [2..]))
=> 1 + (1 + length (takeWhile (<9) (map (^2) [2..])))
=> 1 + (1 + length (takeWhile (<9) (map (^2) 2: [3..])))
=> 1 + (1 + length (takeWhile (<9) (2^2: map (^2) [3..])))
=> 1 + (1 + length (takeWhile (<9) (4: map (^2) [3..])))
=> 1 + (1 + length (4: takeWhile (<9) (map (^2) [3..])))
=> 1 + (1 + (1 + length (takeWhile (<9) (map (^2) [3..]))))
=> 1 + (1 + (1 + length (takeWhile (<9) (map (^2) 3: [4..]))))
=> 1 + (1 + (1 + length (takeWhile (<9) (3^2: map (^2) [4..]))))
=> 1 + (1 + (1 + length (takeWhile (<9) (9: map (^2) [4..]))))
=> 1 + (1 + (1 + length []))
=> 1 + (1 + (1 + 0))
=> 1 + (1 + 1)
=> 1 + 2
=> 3

-- c) not (seq (7+8+9) (1+1==2))
=> not (seq 24 (1+1==2))
=> not (1+1==2)
=> not (2==2)
=> not True
=> False

-- d) let x = [1,2,3]; y = x++y in take 5 y
=> take 5 x++y
=> take 5 [1,2,3]++y
=> take 5 

-}
-- Tehtävä 2: Piirrä seuraavia lausekkeita vastaavat graafit
{-
-- a) (1+1)*(1+1)

     *
    / \
   +   +
  /\   /\
 1  1 1  1
  
  
-- b) let x = 1+1
--        y = x+x
--    in y - (x+x)

              -
             / \
            +   +
            \\ //
              +
             / \
            1   1

-- c) let num = 7
--        nums = num:nums
--        ls = take num nums
--    in (num+1):ls

                 :
                / \
               +   take
              / \   /  \
            _7   1 /  __:_
           | |_____| |  |_|
           |_________|
             
-}
-- Tehtävä 3: Toteuta funktio reverse' joka kääntää listan. Tee
-- funktiostasi sellainen, että paluuarvon täysin evaluoiminen vaatii
-- lineaarisen ajan. Perustele ajankäyttö myös!

reverse' :: [a] -> [a]
reverse' xs = apureverse [] xs
            where apureverse ys [] = ys
                  apureverse ys (x:xs) = apureverse (x:ys) xs
                  
-- Tämä on lineaarinen, sillä :-operaatio lisää alkion listan alkuun. Se
-- on vakioaikainen ja kasaamme käännettyä listaa kokoajan sen avulla. 
-- Tässä tulee myös n kappaletta hahmonsovituksia n-pituiselle listalle.

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
findIt s = apu s 'a'
  where vokaali x = elem x ['a','e','i','o','u','y','A','E','I','O','U','Y']
        apu (' ':xs) vok = vok
        apu (x:xs) vok
           | vokaali x = apu xs x
           | otherwise = apu xs vok
        
{-tehtävänannossa ei sanottu mitään syötteestä, jossa haluttua tilannetta ei ole.

perustelu vakiotilalle: välivaiheiden koot rajoitettuja, rekursion kutsut
eivät kasvata parametrien pituutta. Syötettä on pakko raahata mukana. 
Välivaiheiden koot ilmenevät seuraavasta:

findIt "eb c"
==> apu "eb c" 'a'
==> apu "b c" 'e'
==> apu " c" 'e'
==> 'e'
         
-}

-- Tehtävä 5: Toteuta vakiotilassa toimiva funktio count, joka laskee
-- montako predikaatin täyttävää alkiota listasta löytyy. Perustele
-- miksi toteutuksesi on vakiotilainen.
--
-- Huom! Toteutuksen tulee olla vakiotilainen ilman optimointeja.(taisiis -O0:lla käännettynä)

count :: (a -> Bool) -> [a] -> Int
count p xs = help p xs 0
  where help :: (a -> Bool) -> [a] -> Int -> Int
        help p [] n = n
        help p (x:xs) n
           | p x = seq n (help p xs (n+1))
           | otherwise = help p xs n

-- vakiotilainen koska seqin avulla lukumäärä evaluoidaan aina kun
-- sitä kasvatetaan. Siksi ei pääse tulemaan tilannetta, jossa parametri n
-- olisi tyyliin ((((((0+1)+1)+1)+1)+1)+1)+1, vaan se on tässä tilanteessa 6+1.


-- Tehtävä 6: Seuraavassa fibonaccin lukuja laskevassa funktiossa on
-- muistivuoto (koita vaikkapa laskea 600000. fibonaccin luku). Korjaa
-- se.
--
-- PS. Voit kopioida funktion ja alla olevan mainin omaan tiedostoonsa
-- ja kääntää sen: näin voit profiloida sitä.

fib :: Integer -> Integer
fib n = go 0 1 n
  where go a b 0 = a
        go a b n = seq (a+b) (go b (a+b) (n-1))

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
avg ds = avg' ds 0 0
       where avg' [] _ 0     = 0
             avg' [] n k     = n/k
             avg' (d:ds) n k = seq (n+k+d) (avg' ds (n+d) (k+1))  


{-
profiloitaessa syötteellä 50 000:
maximum residency: alle 150 000
Total time: 0,06 s
%GC time: 25 %

ja syötteellä 500 000:
maximum residency: noin 3 800 000
Total time: 0,09 s
%GC time: 33,3 %


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
        
{-
countZeros0 [0,1,0]             countZeros1 [0,1,0]
==> go 0 [0,1,0]                ==> go 0 [0,1,0]
==> go 0 0:[1,0]                ==> go 0 0:[1,0]
==> go (0+1) [1,0]              ==> go (if 0==0 then 0+1 else 0) [1,0]
==> go (0+1) 1:[0]              ==> go (if True then 0+1 else 0) [1,0]
==> go (0+1) [0]                ==> go (0+1) [1,0]
==> go (0+1) 0:[]               ==> go (0+1) 1:[0]
==> go ((0+1)+1) []             ==> go (if 1==0 then (0+1)+1 else 0+1) [0]
==> (0+1)+1                     ==> go (if False then (0+1)+1 else 0+1) [0]
==> 1+1                         ==> go (0+1) [0]
==> 2                           ==> go (0+1) 0:[]
                                ==> go (if 0==0 then (0+1)+1 else (0+1)) []
                                ==> go (if True then (0+1)+1 else (0+1)) []
                                ==> go ((0+1)+1) []
                                ==> ((0+1)+1)
                                ==> 1+1
                                ==> 2
                                
Huomataan, että countZeros1:ssä if-ehdon evaluointiin menee enemmän aikaa, kuin
countZeros0:ssa kun sama ehto testataan hahmonsovituksella. Siis countZeros0 on
tehokkaampi hahmonsovituksen ansiosta.                                
                                
-}
        
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
        
-- Tehtävä 11: Tee funktio analyze, joka laskee merkkijonossa
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