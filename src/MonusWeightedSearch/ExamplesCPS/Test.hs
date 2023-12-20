-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all -dsuppress-uniques -fforce-recomp -O2 #-}
module MonusWeightedSearch.ExamplesCPS.Test where

import MonusWeightedSearch.ExamplesCPS.Dijkstra
-- import MonusWeightedSearch.Internal.AdjList
import Data.Monus.Dist
-- import Control.DeepSeq as DS

-- onG :: (Graph Word -> Graph Word) -> AdjList -> AdjList
-- onG f al = fromGraph (alSize al) (f (toGraph al))
-- {-# INLINE onG #-}


dijkstraBench :: (Int -> [(Int, Dist)]) -> Int -> Dist
dijkstraBench n k0 = sum $ map snd $ take 10 $ dijkstra n k0
