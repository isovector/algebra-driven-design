{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Scavenge.Spec where

import Control.Monad
import Control.Arrow
import Data.Foldable
import Data.Map.Monoidal (singleton)
import QuickSpec
import Scavenge.Initial
import Scavenge.InputFilter
import Scavenge.Test
import Scavenge.ClueState
import Scavenge.Results
import Test.QuickCheck hiding (within)


newtype Rejoin a b = Rejoin (a, b)

instance (Monoid a, Observe t (a, c) b) => Observe t (a, c) (Rejoin a b) where
  observe t (Rejoin (a, b)) = join (a, observe t b)




spec :: [(Int, Property)]
spec =
  [ (100, ) $ property $ \(is :: [Test])
                          (c :: Challenge Test TestClue TestReward) ->
      runChallenge c is
        === fmap isEmpty (pumpChallenge c is)

  , (1, ) $ property $ \ (k :: TestClue)
                          (is :: [Test]) ->
        getClues @Test (clue @_ @_ @TestReward [k] bottom) is ===
          singleton [k] seen

  , (31, ) $ property $ \ (kctx :: [TestClue])
                          (i :: Maybe Test)
                          (c1 :: Challenge Test TestClue TestReward)
                          (c2 :: Challenge Test TestClue TestReward) ->
      not (isEmpty c1) ==>
        (step kctx i (andThen c1 c2)) =~= (step kctx Nothing =<< (andThen <$> step kctx i c1 <*> pure c2))

  , (40, ) $ property $ \ (k :: [TestClue])
                          (f :: InputFilter Test)
                          (c :: Challenge Test TestClue TestReward) ->
      getClues (eitherC empty (clue k (gate f c))) [] ===
        (fmap (<> failed) $ getClues (clue k (gate f c)) [])

 , (4, ) $ property $ \ (cl :: [TestClue])
                        (ch :: Challenge Test TestClue TestReward) ->
      getRewards (clue cl ch) =~= getRewards ch

 , (5, ) $ property $ \ (r :: TestReward)
                        (is :: [Test]) ->
      getRewards @Test @TestClue (reward r) is == r

 , (6, ) $ property $ \ (f :: i)
                        (c :: Challenge Test TestClue TestReward)
                        (i :: Test) (is :: [Test]) ->
     matches f i ==>
      getRewards (gate f c) (i : is) === getRewards c is

  , (7, ) $ property $ \ (f :: i)
                         (c :: Challenge Test TestClue TestReward)
                         (i :: Test)
                         (is :: [Test]) ->
      not (matches f i) ==>
        getRewards (gate f c) (i : is)
          == getRewards (gate f c) is

  , (54, ) $ property $ \ (c1 :: Challenge Test TestClue TestReward)
                          (c2 :: Challenge Test TestClue TestReward) ->
      both c1 c2 =~= both c2 c1

  , (55, ) $ property $ \ (c1 :: Challenge Test TestClue TestReward)
                          (c2 :: Challenge Test TestClue TestReward)
                          (c3 :: Challenge Test TestClue TestReward) ->
      both c1 (both c2 c3) =~= both (both c1 c2) c3

  , (57, ) $ property $ \ (c1 :: Challenge Test TestClue TestReward)
                          (c2 :: Challenge Test TestClue TestReward)
                          (is :: [Test]) ->
      getRewards (both c1 c2) is
        =~= (getRewards c1 is <> getRewards c2 is)

  , (58, ) $ property $ \ (c :: Challenge Test TestClue TestReward) ->
      both empty c =~= c

  , (59, ) $ property $ \ (c :: Challenge Test TestClue TestReward) ->
      c =~= both c empty

  , (60, ) $ property $ \ (f :: i)
                          (c1 :: Challenge Test TestClue TestReward)
                          (c2 :: Challenge Test TestClue TestReward) ->
      andThen (gate f c1) c2 =~= gate f (andThen c1 c2)

  , (12, ) $ property $ \ (c1 :: Challenge Test TestClue TestReward)
                          (c2 :: Challenge Test TestClue TestReward)
                          (c3 :: Challenge Test TestClue TestReward) ->
      andThen c1 (andThen c2 c3) =~= andThen (andThen c1 c2) c3

  , (13, ) $ property $ \ (c :: Challenge Test TestClue TestReward) ->
      andThen empty c =~= c

  , (14, ) $ property $ \ (c :: Challenge Test TestClue TestReward) ->
      c =~= andThen c empty

--   , (24, ) $ property $ \ (r :: TestReward)
--                           (c1 :: Challenge Test TestClue TestReward)
--                           (c2 :: Challenge Test TestClue TestReward) ->
--       both (andThen (reward r) c1) c2 =~= andThen (reward r) (both c1 c2)

  , (25, ) $ property $ \ (kctx :: [TestClue])
                          (i :: Maybe Test)
                          (c1 :: Challenge Test TestClue TestReward)
                          (c2 :: Challenge Test TestClue TestReward) ->
      step kctx i (both c1 c2) =~= (both <$> step kctx i c1 <*> step kctx i c2)

  , (26, ) $ property $ \ (kctx :: [TestClue])
                          (i :: Maybe Test) ->
      step @Test @_ @TestReward kctx i empty =~= pure empty

  , (27, ) $ property $ \ (kctx :: [TestClue])
                          (i :: Maybe Test)
                          (r :: TestReward) ->
      step kctx i (reward r) =~= (Results r mempty, empty)


  , (28, ) $ property $ \ (i :: Test)
                          (kctx :: [TestClue])
                          (f :: InputFilter Test)
                          (c :: Challenge Test TestClue TestReward) ->
      matches f i ==>
        step kctx (Just i) (gate f c) =~= step kctx Nothing c

  , (29, ) $ property $ \ (kctx :: [TestClue])
                          (i :: Test)
                          (f :: InputFilter Test)
                          (c :: Challenge Test TestClue TestReward) ->
      not (matches f i) ==>
        step kctx (Just i) (gate f c) =~= pure (gate f c)

  , (30, ) $ property $ \ (kctx :: [TestClue])
                          (f :: InputFilter Test)
                          (c :: Challenge Test TestClue TestReward) ->
      step kctx Nothing (gate f c) =~= pure (gate f c)

  , (34, ) $ property $ \ (c1 :: Challenge Test TestClue TestReward)
                          (c2 :: Challenge Test TestClue TestReward)
                          (c3 :: Challenge Test TestClue TestReward) ->
      eitherC c1 (eitherC c2 c3) =~= eitherC (eitherC c1 c2) c3

  , (35, ) $ property $ \ (c1 :: Challenge Test TestClue TestReward)
                          (c2 :: Challenge Test TestClue TestReward) ->
      eitherC c1 c2 =~= eitherC c2 c1

  , (36, ) $ property $ \ (r :: TestReward)
                          (c1 :: Challenge Test TestClue TestReward)
                          (c2 :: Challenge Test TestClue TestReward) ->
      eitherC (andThen (reward r) c1) c2 =~=
        andThen (reward r) (eitherC c1 c2)

  , (38, ) $ property $ \ (k :: TestClue)
                          (is :: [Test]) ->
        getClues @Test (clue @_ @_ @TestReward [k] empty) is ===
          singleton [k] completed

  , (41, ) $ property $ \ (k :: TestClue)
                          (is :: [Test]) ->
        getClues @Test @TestClue @TestReward (clue [k] empty) is =~=
          singleton [k] completed

  , (48, ) $ property $
      reward @Test @TestClue @TestReward mempty
        =~= empty

  , (49, ) $ property $ \ (r1 :: TestReward)
                          (r2 :: TestReward) ->
      andThen (reward @Test @TestClue @TestReward r1) (reward r2)
        =~= reward (r1 <> r2)

  ]


main :: IO ()
main = traverse_ (quickCheckWith stdArgs{maxSuccess=200} . uncurry counterexample . first show) spec


