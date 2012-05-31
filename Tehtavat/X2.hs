module X2 where

import Control.Parallel
import System.Environment

-- Tehtävä 1: Toteuta funktio parMax, joka laskee listan maksimin
-- rinnakkaisesti. Funktiosi tarvitsee käyttää vain kahta ydintä
-- tehokkaasti.
--
-- Vihje: jaa lista kahtia

parMax :: Ord a => [a] -> a
parMax xs = let
                a = maximum $ take ((length xs) `div` 2) xs
                b = maximum $ drop ((length xs) `div` 2) xs
            in 
                a `par` (b `pseq` (max a b))

-- Tehtävä 2: Tehtävänäsi on laskea puussa olevien arvojen summa
-- rinnakkaisesti. Alla on määritelty tyyppi Tree, joka esittää
-- binääripuuta joka sisältää Integer arvoja. Toteuta puun arvojen
-- summan laskeva funktio sumTree siten, että se rinnakkaistuu
-- tehokkaasti ainakin neljälle ytimelle.
--
-- Muista varmistaa että rinnakkaisuus toimii +RTS -s:llä kuten
-- materiaalissakin!
--
-- HUOM! Toteuta rinnakkaisuus käsin par:lla ja pseq:llä. Älä käytä
-- Strategies-modulia.

data Tree = Leaf | Node Integer Tree Tree

sumTree :: Tree -> Integer
sumTree t = undefined
{-
-- Tehtävä 3: Määrittele seuraavat strategiat (käyttäen muita
-- strategioita mikäli mahdollista...)

-- strat1 evaluoi kaksiulotteisen listan alkiot rinnakkain heikkoon
-- päämuotoon.
strat1 :: Strategy [[a]]
strat1 = undefined

-- strat2 evaluoi listan pareja niin että parien sisällä olevat arvot
-- evaluoidaan heikkoon päämuotoon. Evaluointi tapahtuu täysin
-- sarjallisesti.
strat2 :: Strategy [(a,b)]
strat2 = undefined

-- strat3 evaluoi puun solmuissa olevat arvot rinnakkain (tähän
-- tarvitset Eval-monadia)
strat3 :: Strategy Tree
strat3 = undefined

-- Voit testata strat3:a esimerkiksi seuraavien funktioitten avulla:

sumTree' Leaf = 0
sumTree' (Node x l r) = x + sumTree' l + sumTree' r

mk 0 = Leaf
mk 1 = Leaf
mk k = Node (fib k) (mk (k-1)) (mk (k-2))

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

f k = sumTree' (mk k `using` strat3)

-- Tehtävä 4: Tee ohjelma Sum.hs, joka laskee monellako tavalla
-- annettu luku voidaan saada laskemalla yhteen annetun listan
-- alkioita.
--
-- Tee ohjelmastasi mahdollisimman hyvin rinnakkaistuva. Saat käyttää
-- vapaasti Control.Parallel ja Control.Parallel.Strategies-moduleita.
--
-- Tässä runko ohjelmallesi. Näin toteutettua pääohjelmaa voit kutsua
-- seuraavasti:
--
-- ./Sum 3 [1,2,3]
--
-- Homma toimii jos et kirjoita välilyöntejä listan sisään.
--
-- Voit palauttaa tiedoston Sum.hs palautusjärjestelmään erillisesti.
-- Ratkaisua ei tarvitse kopypasteta tähän.

{-
module Main where

import System.Environment

count :: Int -> [Int] -> Int
count = undefined

main = do
  [n',list'] <- getArgs
  let n = read n'
      list = read list'
  print $ count n list
-}

-- Tehtävä 5: Toteuta funktio printFile joka lukee tulostaa tiedoston
-- sisällön käyttäen kahta säiettä: toinen lukee tiedostoa rivi
-- kerrallaan ja toinen tulostaa rivit ruudulle. Käytä säikeitten
-- väliseen viestintään MVaria.

printFile :: FilePath -> IO ()
printFile path = undefined

-- Tehtävä 6: Toteuta MVarien ja säikeitten avulla funktio concMap,
-- joka toimii hieman kuten map, mutta jokaiselle listan alkiolle
-- käynnistetään oma säie. Säikeitten tuottamat arvot kerätään
-- tuloslistaan mielivaltaisessa järjestyksessä (esim.
-- valmistumisjärjestyksessä).
--
-- Huom! Muista että MVarit ovat laiskoja. Ratkaise tämä ongelma jotenkin.
--
-- Alla on testi-main, jolla voit mitata suorituskykyä.

concMap :: (a->b) -> [a] -> IO [b]
concMap f xs = undefined

{-
main = do out <- concMap fib [34,35,34,35,34]
          print out
  where fib 0 = 0
        fib 1 = 1
        fib n = fib (n-1) + fib (n-2)
-}

-- Tehtävä 7: Toteuta funktio concFilter, joka toimii hieman kuten
-- filter, mutta jokaiselle listan alkiolle käynnistetään oma säie.
-- Palautusarvot saavat jälleen olla mielivaltaisessa järjestyksessä.
--
-- Saat käytää MVareja ja Chaneja.
--
-- Huom! Tärkeimpänä ongelmana tässä on että miten varmistat että
-- kaikki säikeet ovat valmiita!

concFilter :: (a -> Bool) -> [a] -> IO [a]
concFilter f xs = undefined

-- Tehtävä 8: Toteuta ohjelma ConcSum, joka tekee samaa kuin
-- Sum.hs, mutta tällä kertaa käytä Control.Concurrentin palveluita.
--
-- Tee jälleen ohjelmastasi mahdollisimman hyvin rinnakkaistuva.
--
-- Voit palauttaa tiedoston ConcSum palautusjärjestelmään.

-- Tehtävä 9&10: Laske vapaavalintaisella tavalla ratkaisu nk.
-- SET-COVER ongelmaan. Palauta ohjelmasi nimellä SetCover.hs
-- palautusjärjestelmään.
--
-- Ohjelmasi saa stdiniin kaksiulotteisen Haskell-listan. Sanotaan
-- tämän listan alkioita _kasoiksi_. Ohjelmasi tulee selvittää, mikä
-- on pienin joukko kasoja X, jolla on seuraava ominaisuus:
--
--   Jokainen alkio joka on jossain kasassa, on myös jossain X:ään
--   kuuluvassa kasassa
--
-- Toisin sanoen sinun tulee selvittää mikä on pienin joukko kasoja
-- joka _peittää_ kaikki tunnetut alkiot.
--
-- Riittää, että ohjelmasi tulostaa pienimmän joukon koon, itse kasoja
-- ei tarvitse tulostaa.
--
-- Tee ohjelmastasi mahdollisimman rinnakkainen (muuten tehokkuudella
-- ei ole niin väliä).
--
-- Tässä main avuksesi:

{-
main = do
  s <- getContents
  let kasat = read s
  print $ ratkaise kasat
-}

-- Kurssisivulta löydät kasan testisyötteitä. Tiedostonimi sc_X_Y
-- tarkoittaa että tiedostossa on X kasaa ja oikea vastaus on Y.
-}
