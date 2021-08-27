module Data.WeightedGraph (Graph) where

import Data.Monus.Dist

type Graph a = a -> [(a, Dist)]
