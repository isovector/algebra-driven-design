{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Workflow.WIP2 where

import Data.Foldable
import qualified Data.Set as S
import Data.Set (Set)
import Data.Data
import Data.Word
import GHC.Generics
import Test.QuickCheck hiding (Result)
import Control.Monad.Writer
import Data.Tuple (swap)
import Data.List
import QuickSpec

data Event = Event Word8
  deriving stock (Eq, Ord, Show, Data, Generic)

-- # ArbitraryEvent
instance Arbitrary Event where
  arbitrary = Event <$> arbitrary
  shrink    = genericShrink


data EventFilter
  = Always
  | Never
  | Exactly Word8  -- ! 1
  deriving stock (Eq, Ord, Show, Data, Generic)

-- # ArbitraryEventFilter
instance Arbitrary EventFilter where
  arbitrary = frequency
    [ (3, pure Always)
    , (1, pure Never)
    , (5, Exactly <$> arbitrary)
    ]
  shrink = genericShrink

always :: EventFilter
always = Always

never :: EventFilter
never = Never

sig_filters :: Sig
sig_filters = signature
  [ con "always" always
  , con  "never" never
  ]


data Reward = Reward Word8
  deriving stock (Eq, Ord, Show, Data, Generic)

-- # ArbitraryReward
instance Arbitrary Reward where
  arbitrary = Reward <$> arbitrary
  shrink    = genericShrink


data Result
  = Victory
  | Defeat
  deriving stock (Eq, Ord, Show, Data, Generic)

-- # ArbitraryResult
instance Arbitrary Result where
  arbitrary = elements [ victory, defeat ]
  shrink    = genericShrink

victory :: Result
victory = Victory

defeat :: Result
defeat = Defeat

sig_results :: Sig
sig_results = signature
  [ con "victory" victory
  , con "defeat"  defeat
  ]


------------------------------------------------------------------------------
--                         constructors
------------------------------------------------------------------------------

data Game
  = Win
  | Lose
  | RewardThen (Set Reward) Game
  | Subgame Game Game Game
  | EitherW Game Game
  | Both Game Game
  | Race Game Game
  | Multigate [(EventFilter, Game)]
  deriving stock (Eq, Ord, Show, Data, Generic)

-- # ArbitraryGame
instance Arbitrary Game where
  arbitrary = sized $ \n ->
    case n <= 1 of
      True -> elements [win, lose]
      False -> frequency
        [ (3, pure win)
        , (3, pure lose)
        , (3, reward  <$> arbitrary)
        , (5, rewardThen <$> arbitrary
                         <*> decayArbitrary 2)
        , (5, andThen <$> decayArbitrary 2
                      <*> decayArbitrary 2)
        , (5, subgame <$> decayArbitrary 3
                      <*> decayArbitrary 3
                      <*> decayArbitrary 3)
        , (5, both <$> decayArbitrary 2
                   <*> decayArbitrary 2)
        , (5, eitherG <$> decayArbitrary 2
                      <*> decayArbitrary 2)
        , (5, race <$> decayArbitrary 2
                   <*> decayArbitrary 2)
        , (5, multigate <$> decayArbitrary 5)
        , (2, comeback  <$> arbitrary)
        , (1, pure bottom)
        , (5, gate <$> arbitrary <*> arbitrary)
        ]
  shrink = genericShrink

-- # ObserveGame
instance
    Observe [Event] (Set Reward, Maybe Result) Game
    where
  observe = runGame

decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary

reward :: Reward -> Game
reward r = rewardThen (S.singleton r) win

rewardThen :: Set Reward -> Game -> Game
rewardThen = RewardThen

win :: Game
win = Win

lose :: Game
lose = Lose

andThen :: Game -> Game -> Game
andThen g1 g2 = subgame g1 g2 lose

subgame :: Game -> Game -> Game -> Game
subgame (RewardThen r g) g1 g2 =
  rewardThen r (subgame g g1 g2)
subgame Win  g1 _  = g1
subgame Lose _  g2 = g2
subgame g    g1 g2 = Subgame g g1 g2

eitherG :: Game -> Game -> Game
eitherG (RewardThen r g1) g2 =
  rewardThen r (eitherG g1 g2)
eitherG g1 (RewardThen r g2) =
  rewardThen r (eitherG g1 g2)
eitherG Lose Lose = lose
eitherG Win  _    = win
eitherG _    Win  = win
eitherG a    b    = EitherW a b

both :: Game -> Game -> Game
both (RewardThen r g1) g2 = rewardThen r (both g1 g2)
both g1 (RewardThen r g2) = rewardThen r (both g1 g2)
both Win  Win  = win
both Lose _    = lose
both _    Lose = lose
both a    b    = Both a b

race :: Game -> Game -> Game
race (RewardThen r g1) g2 = rewardThen r (race g1 g2)
race g1 (RewardThen r g2) = rewardThen r (race g1 g2)
race Win  _ = win
race Lose _ = lose
race _ Win  = win
race _ Lose = lose
race a b    = Race a b

multigate :: [(EventFilter, Game)] -> Game
multigate cs = Multigate cs

sig_games_core :: Sig
sig_games_core = signature
  [
    con        "win" win
  , con       "lose" lose
  -- , con    "subgame" subgame
  -- , con    "eitherG" eitherG
  -- , con       "both" both
  , con       "race" race
  -- , con  "multigate" multigate
  , con "rewardThen" rewardThen
  -- , con     "gate" gate
  ]

------------------------------------------------------------------------------
--                         extensions
------------------------------------------------------------------------------

comeback :: Game -> Game
comeback g = subgame g lose win

bottom :: Game
bottom = multigate []

gate :: EventFilter -> Game -> Game
gate ef g = multigate [(ef, g)]

sig_games_ext :: Sig
sig_games_ext = signature
  [ con "comeback" comeback
  , con   "bottom" bottom
  , con  "andThen" andThen
  , con   "reward" reward
  ]


bingo :: [[Game]] -> Reward -> Game
bingo squares r
  = let subgames = squares
                ++ transpose squares  -- ! 1
        allOf :: [Game] -> Game
        allOf = foldr both    win
        anyOf :: [Game] -> Game
        anyOf = foldr eitherG lose
     in subgame (anyOf (fmap allOf subgames)) (reward r) lose

------------------------------------------------------------------------------
--                           tests
------------------------------------------------------------------------------

bingo_game :: Game
bingo_game = flip bingo (Reward 100) $ do
  x <- [0..2]
  pure $ do
    y <- [0..2]
    pure $ gate (Exactly $ x * 10 + y) win


foo :: Property
foo = property $ \g g2 -> race g g2 =~= race g2 g

------------------------------------------------------------------------------
--                         observations
------------------------------------------------------------------------------

runGame :: [Event] -> Game -> (Set Reward, Maybe Result)
runGame evs g =
  swap $ runWriter $ fmap _toResult $ _runGame g evs

_toResult :: Game -> Maybe Result
_toResult Win  = Just Victory
_toResult Lose = Just Defeat
_toResult _    = Nothing

_runGame :: Game -> [Event] -> Writer (Set Reward) Game
_runGame g (e : es) = do
  g' <- _stepGame g (Just e)
  _runGame g' es
_runGame g [] = do
  g' <- _stepGame g Nothing
  case g == g' of  -- ! 1
    True  -> pure g'
    False -> _runGame g' []

_stepGame :: Game -> Maybe Event -> Writer (Set Reward) Game
_stepGame Win  _ = pure win
_stepGame Lose _ = pure lose

-- # _stepGameRewardThen
_stepGame (RewardThen r g) e =
  tell r >> _stepGame g e

_stepGame (Subgame g g1 g2) e =  -- ! 1
  subgame <$> _stepGame g e      -- ! 2
          <*> pure g1
          <*> pure g2
_stepGame (EitherW g1 g2) e =
  eitherG <$> _stepGame g1 e
          <*> _stepGame g2 e
_stepGame (Both g1 g2) e =
  both <$> _stepGame g1 e
       <*> _stepGame g2 e
_stepGame (Race g1 g2) e =
  race <$> _stepGame g1 e
       <*> _stepGame g2 e
_stepGame (Multigate cs) (Just e)
  | Just (_, g) <- find (\(ef, _) -> matches ef e) cs
  = pure g
_stepGame x@Multigate{} _ = pure x


matches :: EventFilter -> Event -> Bool
matches Never  _ = False
matches Always _ = True
matches (Exactly e) (Event ev) = e == ev

------------------------------------------------------------------------------
--                         specifications
------------------------------------------------------------------------------

sig_types :: Sig
sig_types = signature
  [ monoType        $ Proxy @Event
  , monoType        $ Proxy @EventFilter
  , monoType        $ Proxy @Reward
  , monoType        $ Proxy @Result
  , monoType        $ Proxy @(Set Reward)
  , monoTypeObserve $ Proxy @Game
  , vars ["e"]      $ Proxy @Event
  , vars ["ef"]     $ Proxy @EventFilter
  , vars ["r"]      $ Proxy @Reward
  , vars ["res"]    $ Proxy @Result
  , vars ["g"]      $ Proxy @Game
  ]

sig_background :: Sig
sig_background = background
  [ con "<>" $ (<>) @(Set Reward)
  , con "mempty" $ mempty @(Set Reward)
  ]

sig_options :: Sig
sig_options = signature
  [ withMaxTermSize 5
  ]

sig_preds :: Sig
sig_preds = const mempty $
  [ predicate "isNotReward" $ \case
      RewardThen _ _ -> False
      _ -> True
  ]


main :: IO ()
-- main = traverse_ (quickCheckWith stdArgs {maxSuccess=100000} . uncurry counterexample) quickspec_laws
main = quickSpec $ mconcat
  [ sig_types
  , sig_results
  , sig_games_core
  , sig_background
  , sig_preds
  -- , withLinearity Linear
  , withMaxTermSize 8
  -- , withPruningTermSize 1
  ]



quickspec_laws :: [(String, Property)]
quickspec_laws =
  [ ("yo", property $ \r g1 g2 -> race (rewardThen r g1) g2 =~= rewardThen r (race g1 g2))
  ]


