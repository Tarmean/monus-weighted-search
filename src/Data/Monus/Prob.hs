--------------------------------------------------------------------------------
-- |
-- Module      : Data.Monus.Prob
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- A 'Monus' for probability.
--------------------------------------------------------------------------------

module Data.Monus.Prob where

import Data.Ratio ( Ratio, (%) )
import Data.Monus ( Monus(..) )
import Numeric.Natural ( Natural )
import Data.Ord ( Down(Down) )
import Data.Monoid ( Product(Product) )
import Test.QuickCheck
    ( Arbitrary(arbitrary), NonNegative(NonNegative) )
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import Control.Applicative ( Applicative(liftA2) )
import Control.DeepSeq ( NFData )

-- | A 'Monus' for probabilities, where the underlying 'Monoid' is the product
-- monoid.
--
-- __NB__: The order on this type is reversed from the "usual" order on
-- probability. i.e.
--
-- >>> 0.8 < (0.4 :: Prob)
-- True
newtype Prob = Prob { runProb :: Ratio Natural }
  deriving stock (Eq, Data, Generic, Typeable)
  deriving (Num, Fractional, Show, Read, Real, RealFrac, NFData) via Ratio Natural
  deriving Ord via Down (Ratio Natural)
  deriving (Semigroup, Monoid) via Product (Ratio Natural)

instance Monus Prob where
  x |-| y = case compare x y of
    LT -> y / x
    EQ -> 1
    GT -> x / y
  {-# INLINE (|-|) #-}

instance Arbitrary Prob where
  arbitrary = liftA2 f arbitrary arbitrary
    where
      f (NonNegative 0) (NonNegative 0) = Prob 1
      f (NonNegative n) (NonNegative d) = Prob (fromInteger n % fromInteger (n + d))

instance Bounded Prob where
  minBound = 0
  maxBound = 1
