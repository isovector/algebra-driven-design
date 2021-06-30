{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Scavenge.Test where

import Data.Semigroup.Cancellative
import Data.List
import Test.QuickCheck
import QuickSpec
import Data.MultiSet (MultiSet, fromList, toList)

type TestReward = MultiSet Int

instance Commutative TestReward
instance Arbitrary TestReward where
  arbitrary = fromList <$> arbitrary
  shrink = fmap fromList . sortOn length . genericShrink . toList
instance Observe () TestReward TestReward where

type TestClue = Int

