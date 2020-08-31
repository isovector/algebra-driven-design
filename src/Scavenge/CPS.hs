{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoOverloadedStrings   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StrictData            #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Scavenge.CPS
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

import           Control.Applicative (liftA2)
import           Control.Monad.ST
import           Data.DList (DList)
import qualified Data.DList as DL
import           Data.Foldable
import           Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as M
import           Data.Monoid
import           Data.Monoid.Cancellative
import           Data.MultiSet (MultiSet)
import           Data.STRef
import           Data.Set (Set)
import qualified Data.Set as S
import           GHC.Generics
import           Generic.Data
import           QuickSpec
import           Scavenge.ClueState
import           Scavenge.InputFilter
import           Scavenge.Results
import           Scavenge.Test ()
import           Test.QuickCheck hiding (Result, choose)

newtype Challenge i k r = Challenge
  { unChallenge
        :: forall s  -- ! 1
         . DList k  -- kctx
        -> (DList k -> ClueState
                    -> ST s ClueState)
        -> ST s (ChallengeData i k r s)
        -> ST s (ChallengeData i k r s)
  }

instance ( Show (CustomFilter i), Ord (CustomFilter i)
         , Ord k, Show k
         , Monoid r, Show r
         )
      => Show (Challenge i k r) where
  show (Challenge g) =
    runST $ fmap show $ g mempty (const $ pure . id) end

-- # ArbitraryChallenge
instance
      ( Arbitrary (CustomFilter i), Ord (CustomFilter i)
      , Arbitrary k, Ord k
      , Monoid r, Commutative r, Arbitrary r, Eq r
      ) => Arbitrary (Challenge i k r) where
  arbitrary = sized $ \n ->
    case n <= 1 of
      True -> pure empty
      False -> frequency
        [ (3, pure empty)
        , (3, reward  <$> arbitrary)
        , (3, clue    <$> arbitrary <*> arbitrary)
        , (5, andThen <$> decayArbitrary 2
                      <*> decayArbitrary 2)
        , (5, both <$> decayArbitrary 2
                   <*> decayArbitrary 2)
        , (5, eitherC <$> decayArbitrary 2
                      <*> decayArbitrary 2)
        , (5, gate <$> arbitrary <*> arbitrary)
        , (2, pure bottom)
        ]

-- # ObserveChallenge
instance
      ( HasFilter i, Arbitrary i, Ord (CustomFilter i)
      , Ord k
      , Monoid r, Ord r
      ) => Observe [i]
                   (Results k r, Bool)
                   (Challenge i k r) where
  observe = runChallenge

-- # SemigroupChallenge
instance (Semigroup r, Ord k, Ord (CustomFilter i))
      => Semigroup (Challenge i k r) where
  Challenge c1 <> Challenge c2 =
    Challenge $ \kctx rec cont -> do
        d1 <- c1 kctx rec cont
        d2 <- c2 kctx rec cont
        pure $ d1 <> d2
  {-# INLINABLE (<>) #-}

-- # MonoidChallenge
instance (Monoid r, Ord k, Ord (CustomFilter i))
      => Monoid (Challenge i k r) where
  mempty = Challenge $ \_ -> pure mempty


data ChallengeData i k r s = ChallengeData
  { waitingOn
      :: !(MonoidalMap
            (InputFilter i)
            (ST s (ChallengeData i k r s)))
  , results    :: !(Results k r)
  , isComplete :: !Any
  }
  deriving stock (Generic)


-- # SemigroupCData
deriving via Generically (ChallengeData i k r s)
  instance (Ord k, Semigroup r, Ord (CustomFilter i))
    => Semigroup (ChallengeData i k r s)

-- # MonoidCData
deriving via Generically (ChallengeData i k r s)
  instance (Ord k, Monoid r, Ord (CustomFilter i))
    => Monoid (ChallengeData i k r s)

instance (Show k, Show (CustomFilter i), Show r)
      => Show (ChallengeData i k r s) where
  show (ChallengeData ri r (Any res)) = mconcat
    [ "Challenge { waitingFor = "
    , show $ M.keys ri
    , ", result = "
    , show res
    , ", rewards = "
    , show r
    , " }"
    ]

empty :: Challenge i k r
empty = Challenge $ \_ _ cont -> cont

reward
    :: forall i k r
     . ( Ord k, Ord (CustomFilter i)
       , Commutative r, Monoid r
       )
    => r
    -> Challenge i k r
reward r = rewardThen r empty

tellClue
    :: (Ord (CustomFilter i), Ord k, Monoid r)
    => MonoidalMap [k] ClueState
    -> ChallengeData i k r s
tellClue ks =
  mempty { results = Results mempty ks }

tellReward
    :: (Ord (CustomFilter i), Ord k, Monoid r)
    => r
    -> ChallengeData i k r s
tellReward r = mempty { results = Results r mempty }

clue
    :: forall i k r
     . (Ord (CustomFilter i), Ord k, Monoid r)
    => [k]
    -> Challenge i k r
    -> Challenge i k r
clue [] c = c
clue (k : ks) c =  -- ! 1
  Challenge $ \kctx rec cont -> do
    let kctx' = kctx <> DL.singleton k
        k' = DL.toList kctx'
    state <- rec kctx' seen  -- ! 2
    d <- unChallenge (clue ks c) kctx' rec $ do  -- ! 3
      dc <- cont
      pure $ tellClue (M.singleton k' completed) <> dc
    pure $ tellClue (M.singleton k' state) <> d

rewardThen
    :: forall i k r
     . (Ord (CustomFilter i), Ord k, Monoid r, Ord k)
    => r
    -> Challenge i k r
    -> Challenge i k r
rewardThen r (Challenge c) =
  Challenge $ \kctx rec cont -> do
    d <- c kctx rec cont
    pure $ tellReward r <> d


eitherC
    :: forall i k r
     . (Ord (CustomFilter i), Ord k, Monoid r)
    => Challenge i k r
    -> Challenge i k r
    -> Challenge i k r
eitherC (Challenge c1) (Challenge c2) =
  Challenge $ \kctx rec cont -> do
    filled  <- newSTRef False  -- ! 1
    c1_clues <- newSTRef mempty  -- ! 2
    c2_clues <- newSTRef mempty
    d1 <-
      c1 kctx (decorate filled c1_clues rec) $  -- ! 3
        oneshot filled $ do
          d <- cont
          p <- prune c2_clues  -- ! 4
          pure $ d <> p
    d2 <-
      c2 kctx (decorate filled c2_clues rec) $
        oneshot filled $ do
          d <- cont
          p <- prune c1_clues
          pure $ d <> p
    pure $ d1 <> d2


decorate
    :: Ord k
    => STRef s Bool
    -> STRef s (Set (DList k))
    -> (DList k -> ClueState -> ST s ClueState)
    -> DList k
    -> ClueState
    -> ST s ClueState
decorate filled ref rec k cs = do
  readSTRef filled >>= \case  -- ! 1
    True -> rec k failed  -- ! 2
    False -> do
      modifySTRef' ref $ S.insert k  -- ! 3
      rec k cs


prune
    :: (Ord (CustomFilter i), Ord k, Monoid r)
    => STRef s (Set (DList k))
    -> ST s (ChallengeData i k r s)
prune ref = do
  ks <- readSTRef ref
  pure $ flip foldMap ks $ \k ->
    tellClue $ M.singleton (DL.toList k) failed


oneshot :: Monoid a => STRef s Bool -> ST s a -> ST s a
oneshot ref m =
  readSTRef ref >>= \case
    True  -> pure mempty
    False -> do
      writeSTRef ref True
      m


andThen
    :: Challenge i k r
    -> Challenge i k r
    -> Challenge i k r
andThen (Challenge c1) (Challenge c2) =
  Challenge $ \kctx rec cont ->
    c1 kctx rec (c2 kctx rec cont)

both
    :: forall i k r
     . (Ord (CustomFilter i), Ord k, Monoid r)
    => Challenge i k r
    -> Challenge i k r
    -> Challenge i k r
both (Challenge c1) (Challenge c2) =
  Challenge $ \kctx rec cont -> do
    remaining_wins  <- newSTRef @Int 2  -- ! 1
    let run_win = do  -- ! 2
          modifySTRef' remaining_wins $ subtract 1
          readSTRef remaining_wins >>= \case
            0 -> cont
            _ -> pure mempty
    liftA2 (<>)
      (c1 kctx rec run_win)  -- ! 3
      (c2 kctx rec run_win)

gate
    :: forall i k r
     . (Ord (CustomFilter i), Ord k, Monoid r)
    => InputFilter i
    -> Challenge i k r
    -> Challenge i k r
gate ef (Challenge c) = Challenge $ \kctx rec cont ->
  pure $ (mempty @(ChallengeData i k r _))
    { waitingOn = M.singleton ef $ c kctx rec cont }


bottom
    :: forall i k r
     . (Ord (CustomFilter i), Ord k, Monoid r)
    => Challenge i k r
bottom = Challenge $ \_ -> mempty

end
    :: (Ord (CustomFilter i), Ord k, Monoid r)
    => ST s (ChallengeData i k r s)
end = pure $ mempty { isComplete = Any True }

runChallenge
    :: forall i k r
     . ( HasFilter i, Ord (CustomFilter i)
       , Ord k
       , Monoid r
       )
    => [i] -> Challenge i k r -> (Results k r, Bool)
runChallenge evs (Challenge c) = runST $ do
  d' <-
    pumpChallenge evs =<<
      c mempty               -- ! 1
        (const $ pure . id)  -- ! 2
        end                  -- ! 3
  pure (results d', getAny $ isComplete d')


pumpChallenge
    :: ( HasFilter i, Ord (CustomFilter i)
       , Ord k
       , Monoid r
       )
    => [i]
    -> ChallengeData i k r s
    -> ST s (ChallengeData i k r s)
pumpChallenge [] d = pure d
pumpChallenge _ d
  | getAny $ isComplete d  -- ! 1
  = pure d
pumpChallenge (ri : es) d =
  pumpChallenge es =<< step ri d

getClues
    :: forall i k r.
       ( HasFilter i, Ord (CustomFilter i)
       , Ord k
       , Monoid r
       )
    => Challenge i k r
    -> [i]
    -> MonoidalMap [k] ClueState
getClues c = clues . fst . flip runChallenge c

getRewards
    :: forall i k r.
       ( HasFilter i, Ord (CustomFilter i)
       , Ord k
       , Monoid r
       )
    => Challenge i k r
    -> [i]
    -> r
getRewards c = rewards . fst . flip runChallenge c


step
    :: forall i k r s.
       ( HasFilter i, Ord (CustomFilter i)
       , Ord k
       , Monoid r
       )
    => i
    -> ChallengeData i k r s
    -> ST s (ChallengeData i k r s)
step ri d = do
  let efs = M.assocs $ waitingOn d  -- ! 1
  (endo, ds) <-
    flip foldMapM efs $ \(ef, res) ->  -- ! 2
      case matches ef ri of  -- ! 3
        True -> do
          d' <- res  -- ! 4
          pure (Endo $ M.delete ef, d')  -- ! 5
        False -> mempty
  pure $
    d { waitingOn =
           appEndo endo $ waitingOn d  -- ! 6
       } <> ds  -- ! 7

foldMapM
    :: (Monoid m, Applicative f, Traversable t)
    => (a -> f m)
    -> t a
    -> f m
foldMapM f = fmap fold . traverse f

#include "spec.inc"

{-# INLINABLE empty #-}
{-# INLINABLE reward #-}
{-# INLINABLE tellClue #-}
{-# INLINABLE tellReward #-}
{-# INLINABLE clue #-}
{-# INLINABLE rewardThen #-}
{-# INLINABLE eitherC #-}
{-# INLINABLE decorate #-}
{-# INLINABLE prune #-}
{-# INLINABLE oneshot #-}
{-# INLINABLE andThen #-}
{-# INLINABLE both #-}
{-# INLINABLE gate #-}
{-# INLINABLE bottom #-}

