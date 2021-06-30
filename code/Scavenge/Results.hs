{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Scavenge.Results where

import Test.QuickCheck
import GHC.Generics
import QuickSpec
import Data.Map.Monoidal (MonoidalMap, toList, fromList)
import Generic.Data
import Scavenge.ClueState

data Results k r = Results
  { rewards :: r
  , clues   :: MonoidalMap [k] ClueState
  }
  deriving stock (Eq, Ord, Generic)
  deriving (Semigroup, Monoid)
    via Generically (Results k r)

instance (Show k, Show r) => Show (Results k r) where
  show (Results r k) = mconcat
    [ "Results ("
    , show r
    , ") (fromList "
    , show $ toList k
    , ")"
    ]

instance (Arbitrary k, Ord k, Arbitrary v)
      => Arbitrary (MonoidalMap k v) where
  arbitrary = fromList <$> arbitrary
  shrink = fmap fromList . genericShrink . toList

instance (Ord k, Ord v) => Observe () (MonoidalMap k v) (MonoidalMap k v)

instance (Ord k, Ord r)
      => Observe () (Results k r) (Results k r) where

