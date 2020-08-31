{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Scavenge.Initial
  ( -- * Observations
    runChallenge
  , getClues
  , getRewards

    -- * Challenges
  , empty
  , reward
  , clue
  , andThen
  , both
  , eitherC
  , bottom
  , gate

    -- * Input filters
  , always
  , never
  , andF
  , orF
  , notF
  , custom
  , HasFilter (..)

    -- * Clue states
  , seen
  , completed
  , failed

    -- * Laws
  , quickspec_laws

    -- * Types
  , Challenge ()
  , MonoidalMap ()
  , Results ()
  , ClueState ()
  ) where

import Control.Monad
import Control.Monad.Writer.Class
import Data.Map.Monoidal (MonoidalMap, singleton)
import Data.Semigroup.Cancellative
import GHC.Generics
import Data.MultiSet (MultiSet)
import QuickSpec
import Scavenge.ClueState
import Scavenge.InputFilter
import Scavenge.Results
import Scavenge.Test ()
import Test.QuickCheck hiding (within)


------------------------------------------------------------------------------

data Challenge i k r
  = Empty
  | Gate (InputFilter i) (Challenge i k r)
  | Clue       k (Challenge i k r)
  | RewardThen r (Challenge i k r)
  | EitherC (Challenge i k r) (Challenge i k r)
  | Both    (Challenge i k r) (Challenge i k r)
  | AndThen (Challenge i k r) (Challenge i k r)
  deriving stock (Generic)

deriving stock instance
  (Eq r, Eq k, Eq (CustomFilter i))
    => Eq (Challenge i k r)

deriving stock instance
  (Show r, Show k, Show (CustomFilter i))
    => Show (Challenge i k r)

-- # ArbitraryChallenge
instance
      ( Arbitrary (CustomFilter i)
      , Arbitrary k
      , Monoid r, Commutative r, Arbitrary r, Eq r
      ) => Arbitrary (Challenge i k r) where
  arbitrary = sized $ \n ->
    case n <= 1 of
      True -> pure empty
      False -> frequency
        [ (3, pure empty)
        , (3, reward  <$> arbitrary)
        , (3, clue    <$> resize 4 arbitrary <*> arbitrary)
        , (5, andThen <$> decayArbitrary 2
                      <*> decayArbitrary 2)
        , (5, both <$> decayArbitrary 2
                   <*> decayArbitrary 2)
        , (5, eitherC <$> decayArbitrary 2
                      <*> decayArbitrary 2)
        , (5, gate <$> arbitrary <*> arbitrary)
        , (2, pure bottom)
        ]

  shrink Empty = []
  shrink x = Empty : filter isValid (genericShrink x)

-- # ObserveChallenge
instance
      ( HasFilter i, Arbitrary i, Eq (CustomFilter i)
      , Ord k
      , Commutative r, Monoid r, Ord r
      ) => Observe [i]
                   (Results k r, Bool)
                   (Challenge i k r) where
  observe = flip runChallenge

------------------------------------------------------------------------------


findClues
    :: forall i k r
     . Ord k
    => [k]
    -> Challenge i k r
    -> MonoidalMap [k] ClueState
findClues _    Empty
  = mempty
findClues kctx (Both c1 c2)
  = findClues kctx c1 <> findClues kctx c2
findClues kctx (EitherC c1 c2)
  = findClues kctx c1 <> findClues kctx c2
findClues _    (Gate _ _)
  = mempty
findClues kctx (AndThen c _)
  = findClues kctx c
findClues kctx (RewardThen _ c)
  = findClues kctx c
findClues kctx (Clue k Empty)
  = singleton (kctx <> [k]) completed
findClues kctx (Clue k c)
  = singleton (kctx <> [k]) seen
    <> findClues (kctx <> [k]) c

pumpChallenge
    :: forall i k r
     . ( Ord k
       , HasFilter i
       , Monoid r, Commutative r, Eq r
       )
    => Challenge i k r
    -> [i]
    -> (Results k r, Challenge i k r)
pumpChallenge c
  = foldM (flip $ step []) c
  . (Nothing :)
  . fmap Just

runChallenge
    :: forall i k r.
      ( HasFilter i, Eq (CustomFilter i)
      , Ord k
      , Monoid r, Commutative r, Eq r
      )
    => Challenge i k r
    -> [i]
    -> (Results k r, Bool)
runChallenge c = fmap (== Empty) . pumpChallenge c

getRewards
    :: forall i k r.
      ( HasFilter i
      , Ord k
      , Monoid r, Commutative r, Eq r
      ) =>
      Challenge i k r -> [i] -> r
getRewards c = rewards . fst . pumpChallenge c

getClues
    :: forall i k r.
      ( HasFilter i
      , Ord k
      , Monoid r, Commutative r, Eq r
      )
    => Challenge i k r
    -> [i]
    -> MonoidalMap [k] ClueState
getClues c = clues . fst . pumpChallenge c


isEmpty
    :: forall i k r.
      ( HasFilter i, Eq (CustomFilter i)
      , Ord k
      , Monoid r, Commutative r, Eq r
      )
    => Challenge i k r
    -> Bool
isEmpty = (== Empty) . snd . flip pumpChallenge []

-- # stepEmpty
step
    :: forall i k r
     . ( HasFilter i
       , Ord k
       , Monoid r, Commutative r, Eq r
       )
    => [k]
    -> Maybe i
    -> Challenge i k r
    -> (Results k r, Challenge i k r)
step _    _ Empty = pure empty

-- # stepBoth
step kctx i (Both c1 c2)
  = both <$> step kctx i c1 <*> step kctx i c2

-- # stepEitherC
step kctx i (EitherC c1 c2) = do
  c1' <- step kctx i c1
  c2' <- step kctx i c2
  case (c1', c2') of
    (Empty, _) -> prune kctx c2'
    (_, Empty) -> prune kctx c1'
    _         -> pure $ eitherC c1' c2'

-- # stepAndThen
step kctx i (AndThen c1 c2) =
  step kctx i c1 >>= \case
    Empty -> step kctx Nothing c2
    c1' -> pure $ andThen c1' c2

-- # stepRewardThen
step kctx i (RewardThen r c) = do
  tellReward r
  step kctx i c

-- # stepGate
step kctx (Just i) (Gate f c)
  | matches f i = step kctx Nothing c
step _    _ c@Gate{} = pure c

-- # stepClue
step kctx i (Clue k c) = do
  let kctx' = kctx <> [k]
  step kctx' i c >>= \case
    Empty -> do
      tellClue $ singleton kctx' completed
      pure empty
    c' -> do
      tellClue $ singleton kctx' seen
      pure $ clue [k] c'

prune
    :: (Ord k, Monoid r)
    => [k]
    -> Challenge i k r
    -> (Results k r, Challenge i k r)
prune kctx c = do
  tellClue $ fmap (<> failed) $ findClues kctx c
  pure empty


tellReward
    :: (Ord k, MonadWriter (Results k r) m)
    => r -> m ()
tellReward r = tell $ Results r mempty


tellClue
    :: (Monoid r , MonadWriter (Results k r) m)
    => MonoidalMap [k] ClueState -> m ()
tellClue k = tell $ Results mempty k

------------------------------------------------------------------------------

clue
    :: forall i k r
     . ( Eq r, Monoid r, Commutative r)
    => [k] -> Challenge i k r -> Challenge i k r
clue [] c = c
clue k (RewardThen r c) = rewardThen r (clue k c)
clue k c = foldr Clue c k


reward
    :: forall i k r
     . (Eq r, Monoid r, Commutative r)
    => r -> Challenge i k r
reward r = rewardThen r empty


bottom :: forall i k r. Challenge i k r
bottom = gate never empty


rewardThen
    :: forall i k r
     . (Eq r, Monoid r, Commutative r)
    => r -> Challenge i k r -> Challenge i k r
rewardThen r c | r == mempty = c
rewardThen r' (RewardThen r c) = RewardThen (r <> r') c
rewardThen r c = RewardThen r c


gate
    :: forall i k r
     . InputFilter i
    -> Challenge i k r
    -> Challenge i k r
gate = Gate


both
    :: forall i k r
     . (Eq r, Monoid r, Commutative r)
     => Challenge i k r
     -> Challenge i k r
     -> Challenge i k r
both (RewardThen r c1) c2 = rewardThen r (both c1 c2)
both c1 (RewardThen r c2) = rewardThen r (both c1 c2)
both Empty c2 = c2
both c1 Empty = c1
both c1 c2 = Both c1 c2


empty :: forall i k r. Challenge i k r
empty = Empty


andThen
    :: forall i k r
     . ( Monoid r, Commutative r, Eq r
       )
    => Challenge i k r
    -> Challenge i k r
    -> Challenge i k r
andThen Empty c = c
andThen (Gate f c1) c2 = gate f (andThen c1 c2)
andThen (RewardThen r c1) c2 =
  rewardThen r (andThen c1 c2)
andThen (AndThen c1 c2) c3 =
  andThen c1 (andThen c2 c3)
andThen c1 c2 = AndThen c1 c2


eitherC
    :: forall i k r
     . (Eq r, Monoid r, Commutative r)
    => Challenge i k r
    -> Challenge i k r
    -> Challenge i k r
eitherC (RewardThen r c1) c2 =
  rewardThen r (eitherC c1 c2)
eitherC c1 (RewardThen r c2) =
  rewardThen r (eitherC c1 c2)
eitherC c1 c2 = EitherC c1 c2


isValid
    :: forall i k r
     . Challenge i k r -> Bool
isValid (AndThen Empty _) = False
isValid (Both Empty _) = False
isValid (Both _ Empty) = False
isValid (EitherC _ Empty) = False
isValid (EitherC Empty _) = False
isValid (Both (RewardThen _ _) _) = False
isValid (Both _ (RewardThen _ _)) = False
isValid (EitherC (RewardThen _ _) _) = False
isValid (EitherC _ (RewardThen _ _)) = False
isValid _ = True

#include "spec.inc"

