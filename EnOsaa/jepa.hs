

import Control.Parallel
import System.Environment


main = do
         print (parMax [999999,999998..1])



parMax :: Ord a => [a] -> a
parMax xs = let
                a = maximum $ take ((length xs) `div` 2) xs
                b = maximum $ drop ((length xs) `div` 2) xs
            in
                a `par` (b `pseq` (max a b))
