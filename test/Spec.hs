{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.QuickCheck hiding (tabulate)
import Test.Tasty.QuickCheck hiding (tabulate)
import Test.Tasty

import Data.List (sort)
import Data.Bifoldable

import Control.Monad.Heap.List

import MonusWeightedSearch.Internal.AdjList

import qualified MonusWeightedSearch.Internal.Heap as H
import qualified MonusWeightedSearch.Dijkstra as M

import Data.Monus
import Data.Monus.Prob

prop_monadDijkstra :: AdjList -> Property
prop_monadDijkstra gm = sort (H.dijkstra (toGraph gm) 1) === sort (M.dijkstra (toGraph gm) 1)

prop_probOrdMonoid :: Prob -> Prob -> Property
prop_probOrdMonoid x y = (x <= x <> y) .&&. (y <= x <> y)

prop_probMonus :: Prob -> Prob -> Property
prop_probMonus x y
  | x <= y    = x <> (x |-| y) === y
  | otherwise = y <> (y |-| x) === x

prop_bifoldlListCons :: Property
prop_bifoldlListCons =
  bifoldr (:) (:) [] (True :- False) === reverse (bifoldl (flip (:)) (flip (:)) [] (True :- False))

return []

main :: IO ()
main = defaultMain (testProperties "Properties" $allProperties)
