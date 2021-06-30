{-# LANGUAGE MultiParamTypeClasses #-}

module Scavenge.ClueState where

import Test.QuickCheck
import QuickSpec
import Data.Semigroup

data ClueState
  = Seen | Failed | Completed  -- ! 1
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (Semigroup, Monoid) via Max ClueState  -- ! 2

instance Observe () ClueState ClueState

instance Arbitrary ClueState where
  arbitrary = elements $ enumFromTo minBound maxBound

seen :: ClueState
seen = Seen

completed :: ClueState
completed = Completed

failed :: ClueState
failed = Failed

------------------------------------------------------------------------------

sig_cluestate :: Sig
sig_cluestate = signature
  [ con "seen"      $ seen
  , con "completed" $ completed
  , con "failed"    $ failed
  , con "<>"        $ (<>) @ClueState
  , mono @ClueState
  ]

