{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module ADD.Games.Correct where

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
  | RewardThen Reward Game
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
reward r = rewardThen r win

rewardThen :: Reward -> Game -> Game
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
  [ con        "win" win
  , con       "lose" lose
  , con    "subgame" subgame
  , con    "eitherG" eitherG
  , con       "both" both
  , con       "race" race
  , con  "multigate" multigate
  , con "rewardThen" rewardThen
  , con     "gate" gate
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
  tell (S.singleton r) >> _stepGame g e

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
  , monoTypeObserve $ Proxy @Game
  , vars ["e"]      $ Proxy @Event
  , vars ["ef"]     $ Proxy @EventFilter
  , vars ["r"]      $ Proxy @Reward
  , vars ["res"]    $ Proxy @Result
  , vars ["g"]      $ Proxy @Game
  ]

sig_options :: Sig
sig_options = signature
  [ withMaxTermSize 5
  ]




quickspec_laws' :: [(String, Property)]
quickspec_laws' =
  [ ( "comeback bottom = bottom"
    , property $ comeback bottom =~= bottom)
  , ( "win = comeback lose"
    , property $ win =~= comeback lose)
  , ( "lose = comeback win"
    , property $ lose =~= comeback win)
  , ( "both g g2 = both g2 g"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            both g g2 =~= both g2 g)
  , ( "both g g = g"
    , property $ \ (g :: Game) -> both g g =~= g)
  , ( "eitherG g g2 = eitherG g2 g"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            eitherG g g2 =~= eitherG g2 g)
  , ( "eitherG g g = g"
    , property $ \ (g :: Game) -> eitherG g g =~= g)
  , ( "race g g = g"
    , property $ \ (g :: Game) -> race g g =~= g)
  , ( "andThen g win = g"
    , property $ \ (g :: Game) -> andThen g win =~= g)
  , ( "andThen bottom g = bottom"
    , property $
        \ (g :: Game) -> andThen bottom g =~= bottom)
  , ( "andThen lose g = lose"
    , property $
        \ (g :: Game) -> andThen lose g =~= lose)
  , ( "andThen win g = g"
    , property $ \ (g :: Game) -> andThen win g =~= g)
  , ( "both g bottom = andThen g bottom"
    , property $
        \ (g :: Game) -> both g bottom =~= andThen g bottom)
  , ( "both g win = g"
    , property $ \ (g :: Game) -> both g win =~= g)
  , ( "eitherG g lose = g"
    , property $ \ (g :: Game) -> eitherG g lose =~= g)
  , ( "race g bottom = g"
    , property $ \ (g :: Game) -> race g bottom =~= g)
  , ( "race bottom g = g"
    , property $ \ (g :: Game) -> race bottom g =~= g)
  , ( "race lose g = both g lose"
    , property $
        \ (g :: Game) -> race lose g =~= both g lose)
  , ( "race win g = eitherG g win"
    , property $
        \ (g :: Game) -> race win g =~= eitherG g win)
  , ( "gate ef bottom = bottom"
    , property $
        \ (ef :: EventFilter) -> gate ef bottom =~= bottom)
  , ( "reward r = rewardThen r win"
    , property $
        \ (r :: Reward) -> reward r =~= rewardThen r win)
  , ( "comeback (comeback g) = g"
    , property $
        \ (g :: Game) -> comeback (comeback g) =~= g)
  , ( "comeback (reward r) = rewardThen r lose"
    , property $
        \ (r :: Reward) ->
            comeback (reward r) =~= rewardThen r lose)
  , ( "andThen g g2 = subgame g g2 lose"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            andThen g g2 =~= subgame g g2 lose)
  , ( "subgame bottom g g2 = bottom"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            subgame bottom g g2 =~= bottom)
  , ( "subgame lose g g2 = g2"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            subgame lose g g2 =~= g2)
  , ( "subgame win g g2 = g"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            subgame win g g2 =~= g)
  , ( "comeback g = subgame g lose win"
    , property $
        \ (g :: Game) -> comeback g =~= subgame g lose win)
  , ( "subgame g win bottom = eitherG g bottom"
    , property $
        \ (g :: Game) ->
            subgame g win bottom =~= eitherG g bottom)
  , ( "andThen (comeback g) g2 = subgame g lose g2"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            andThen (comeback g) g2 =~= subgame g lose g2)
  , ( "rewardThen r g = andThen (reward r) g"
    , property $
        \ (g :: Game) (r :: Reward) ->
            rewardThen r g =~= andThen (reward r) g)
  , ( "both g (comeback g) = andThen g lose"
    , property $
        \ (g :: Game) ->
            both g (comeback g) =~= andThen g lose)
  , ( "rewardThen r g = both g (reward r)"
    , property $
        \ (g :: Game) (r :: Reward) ->
            rewardThen r g =~= both g (reward r))
  , ( "eitherG g (comeback g) = subgame g win win"
    , property $
        \ (g :: Game) ->
            eitherG g (comeback g) =~= subgame g win win)
  , ( "race g (comeback g) = g"
    , property $
        \ (g :: Game) -> race g (comeback g) =~= g)
  , ( "race (reward r) g = eitherG g (reward r)"
    , property $
        \ (g :: Game) (r :: Reward) ->
            race (reward r) g =~= eitherG g (reward r))
  , ( "gate ef (comeback g) = comeback (gate ef g)"
    , property $
        \ (ef :: EventFilter) (g :: Game) ->
            gate ef (comeback g) =~= comeback (gate ef g))
  , ( "rewardThen r (comeback g) = comeback (rewardThen r g)"
    , property $
        \ (g :: Game) (r :: Reward) ->
            rewardThen r (comeback g) =~= comeback (rewardThen r g))
  , ( "comeback (andThen g bottom) = subgame g bottom win"
    , property $
        \ (g :: Game) ->
            comeback (andThen g bottom) =~= subgame g bottom win)
  , ( "comeback (andThen g lose) = subgame g win win"
    , property $
        \ (g :: Game) ->
            comeback (andThen g lose) =~= subgame g win win)
  , ( "comeback (both g lose) = eitherG g win"
    , property $
        \ (g :: Game) ->
            comeback (both g lose) =~= eitherG g win)
  , ( "comeback (eitherG g bottom) = subgame g lose bottom"
    , property $
        \ (g :: Game) ->
            comeback (eitherG g bottom) =~= subgame g lose bottom)
  , ( "both lose (comeback g) = both g lose"
    , property $
        \ (g :: Game) ->
            both lose (comeback g) =~= both g lose)
  , ( "both lose (multigate xs) = lose"
    , property $
        \ (xs :: [(EventFilter, Game)]) ->
            both lose (multigate xs) =~= lose)
  , ( "race (comeback g) lose = comeback (race g win)"
    , property $
        \ (g :: Game) ->
            race (comeback g) lose =~= comeback (race g win))
  , ( "race (multigate xs) lose = lose"
    , property $
        \ (xs :: [(EventFilter, Game)]) ->
            race (multigate xs) lose =~= lose)
  , ( "race (multigate xs) win = win"
    , property $
        \ (xs :: [(EventFilter, Game)]) ->
            race (multigate xs) win =~= win)
  , ( "andThen (andThen g g2) g3 = andThen g (andThen g2 g3)"
    , property $
        \ (g :: Game) (g2 :: Game) (g3 :: Game) ->
            andThen (andThen g g2) g3 =~= andThen g (andThen g2 g3))
  , ( "both (both g g2) g3 = both g (both g2 g3)"
    , property $
        \ (g :: Game) (g2 :: Game) (g3 :: Game) ->
            both (both g g2) g3 =~= both g (both g2 g3))
  , ( "eitherG g (andThen g g) = g"
    , property $
        \ (g :: Game) -> eitherG g (andThen g g) =~= g)
  , ( "eitherG g (both g g2) = both g (eitherG g g2)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            eitherG g (both g g2) =~= both g (eitherG g g2))
  , ( "eitherG (eitherG g g2) g3 = eitherG g (eitherG g2 g3)"
    , property $
        \ (g :: Game) (g2 :: Game) (g3 :: Game) ->
            eitherG (eitherG g g2) g3 =~= eitherG g (eitherG g2 g3))
  , ( "eitherG g (rewardThen r g2) = eitherG g2 (rewardThen r g)"
    , property $
        \ (g :: Game) (g2 :: Game) (r :: Reward) ->
            eitherG g (rewardThen r g2) =~= eitherG g2 (rewardThen r g))
  , ( "race g (andThen g g2) = eitherG g (andThen g g2)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            race g (andThen g g2) =~= eitherG g (andThen g g2))
  , ( "race g (both g g2) = both g (race g g2)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            race g (both g g2) =~= both g (race g g2))
  , ( "race g (eitherG g g2) = eitherG g (race g g2)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            race g (eitherG g g2) =~= eitherG g (race g g2))
  , ( "race g (race g g2) = race g g2"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            race g (race g g2) =~= race g g2)
  , ( "race g (race g2 g) = race g g2"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            race g (race g2 g) =~= race g g2)
  , ( "race g (rewardThen r g) = rewardThen r g"
    , property $
        \ (g :: Game) (r :: Reward) ->
            race g (rewardThen r g) =~= rewardThen r g)
  , ( "race (both g g2) g = both g (race g2 g)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            race (both g g2) g =~= both g (race g2 g))
  , ( "race (eitherG g g2) g = eitherG g (race g2 g)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            race (eitherG g g2) g =~= eitherG g (race g2 g))
  , ( "race (race g g2) g3 = race g (race g2 g3)"
    , property $
        \ (g :: Game) (g2 :: Game) (g3 :: Game) ->
            race (race g g2) g3 =~= race g (race g2 g3))
  , ( "race (rewardThen r g) g2 = race g (rewardThen r g2)"
    , property $
        \ (g :: Game) (g2 :: Game) (r :: Reward) ->
            race (rewardThen r g) g2 =~= race g (rewardThen r g2))
  , ( "gate ef (andThen g g2) = andThen (gate ef g) g2"
    , property $
        \ (ef :: EventFilter) (g :: Game) (g2 :: Game) ->
            gate ef (andThen g g2) =~= andThen (gate ef g) g2)
  , ( "subgame (comeback g) g2 g3 = subgame g g3 g2"
    , property $
        \ (g :: Game) (g2 :: Game) (g3 :: Game) ->
            subgame (comeback g) g2 g3 =~= subgame g g3 g2)
  , ( "subgame (reward r) g g2 = rewardThen r g"
    , property $
        \ (g :: Game) (g2 :: Game) (r :: Reward) ->
            subgame (reward r) g g2 =~= rewardThen r g)
  , ( "comeback (subgame g g2 win) = andThen g (comeback g2)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            comeback (subgame g g2 win) =~= andThen g (comeback g2))
  , ( "andThen g (both g lose) = andThen g lose"
    , property $
        \ (g :: Game) ->
            andThen g (both g lose) =~= andThen g lose)
  , ( "andThen g (eitherG g2 win) = eitherG g (andThen g g2)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            andThen g (eitherG g2 win) =~= eitherG g (andThen g g2))
  , ( "andThen g (race g2 win) = race (andThen g g2) g"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            andThen g (race g2 win) =~= race (andThen g g2) g)
  , ( "andThen (eitherG g bottom) g2 = subgame g g2 bottom"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            andThen (eitherG g bottom) g2 =~= subgame g g2 bottom)
  , ( "andThen (eitherG g win) g = g"
    , property $
        \ (g :: Game) -> andThen (eitherG g win) g =~= g)
  , ( "andThen (race g g2) lose = andThen (race g2 g) lose"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            andThen (race g g2) lose =~= andThen (race g2 g) lose)
  , ( "andThen (race g lose) g = race g lose"
    , property $
        \ (g :: Game) ->
            andThen (race g lose) g =~= race g lose)
  , ( "andThen (race g win) g = g"
    , property $
        \ (g :: Game) -> andThen (race g win) g =~= g)
  , ( "both g (eitherG g2 win) = andThen (eitherG g2 win) g"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            both g (eitherG g2 win) =~= andThen (eitherG g2 win) g)
  , ( "both lose (eitherG g g2) = both g (both g2 lose)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            both lose (eitherG g g2) =~= both g (both g2 lose))
  , ( "both lose (race g g2) = both g (both g2 lose)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            both lose (race g g2) =~= both g (both g2 lose))
  , ( "both lose (gate ef g) = lose"
    , property $
        \ (ef :: EventFilter) (g :: Game) ->
            both lose (gate ef g) =~= lose)
  , ( "both (comeback g) (comeback g2) = comeback (eitherG g g2)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            both (comeback g) (comeback g2) =~= comeback (eitherG g g2))
  , ( "eitherG g (both g2 lose) = andThen (eitherG g2 win) g"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            eitherG g (both g2 lose) =~= andThen (eitherG g2 win) g)
  , ( "race g (andThen g2 bottom) = both g (race g g2)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            race g (andThen g2 bottom) =~= both g (race g g2))
  , ( "race g (eitherG g2 bottom) = eitherG g (race g g2)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            race g (eitherG g2 bottom) =~= eitherG g (race g g2))
  , ( "race (comeback g) (comeback g2) = comeback (race g g2)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            race (comeback g) (comeback g2) =~= comeback (race g g2))
  , ( "race (andThen g g) lose = race g lose"
    , property $
        \ (g :: Game) ->
            race (andThen g g) lose =~= race g lose)
  , ( "race (andThen g g) win = race g win"
    , property $
        \ (g :: Game) ->
            race (andThen g g) win =~= race g win)
  , ( "race (andThen g bottom) g2 = both g2 (race g g2)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            race (andThen g bottom) g2 =~= both g2 (race g g2))
  , ( "race (eitherG g bottom) g2 = eitherG g2 (race g g2)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            race (eitherG g bottom) g2 =~= eitherG g2 (race g g2))
  , ( "race (gate ef g) lose = lose"
    , property $
        \ (ef :: EventFilter) (g :: Game) ->
            race (gate ef g) lose =~= lose)
  , ( "race (gate ef g) win = win"
    , property $
        \ (ef :: EventFilter) (g :: Game) ->
            race (gate ef g) win =~= win)
  , ( "gate ef (eitherG g bottom) = eitherG bottom (gate ef g)"
    , property $
        \ (ef :: EventFilter) (g :: Game) ->
            gate ef (eitherG g bottom) =~= eitherG bottom (gate ef g))
  , ( "subgame g bottom (comeback g2) = comeback (subgame g bottom g2)"
    , property $
        \ (g :: Game) (g2 :: Game) ->
            subgame g bottom (comeback g2) =~= comeback (subgame g bottom g2))
  , ( "eitherG bottom (andThen g lose) = subgame g bottom bottom"
    , property $
        \ (g :: Game) ->
            eitherG bottom (andThen g lose) =~= subgame g bottom bottom)
  ]

