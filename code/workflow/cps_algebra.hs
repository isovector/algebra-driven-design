{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NoOverloadedStrings      #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StrictData               #-}
{-# OPTIONS_GHC -fno-warn-orphans     #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Workflow.WIP2 where

import           Control.Applicative (liftA2)
import           Control.Monad.ST
import           Data.Data
import           Data.Foldable
import           Data.List (transpose)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.STRef
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Word
import           GHC.Generics
import           QuickSpec
import           Test.QuickCheck hiding (Result, choose)

newtype Game = Game
  { unGame
        :: forall s  -- ! 1
         . ST s (GameData s)  -- win continuation
        -> ST s (GameData s)  -- lose continuation
        -> ST s (GameData s)
  }

{-

-- # EarlyGame
newtype Game = Game
  { unGame :: GameData -> GameData -> GameData
  }

-}

-- # SemigroupGame
instance Semigroup Game where
  Game g1 <> Game g2  = Game $ \w l -> do
    gd1 <- g1 w l
    gd2 <- g2 w l
    pure $ gd1 <> gd2
  {-# INLINABLE (<>) #-}

-- # MonoidGame
instance Monoid (Game) where
  mempty = Game $ \_ _ -> pure mempty


data GameData s = GameData
  { _waitingOn  :: !(Map EventFilter (ST s (GameData s)))
  , _rewards    :: !(Set Reward)
  , _result     :: !(First Result)
  } deriving stock (Generic)

{-

-- # EarlyGameData
data GameData = GameData
  { _waitingOn :: Map EventFilter GameData
  , _rewards   :: Set Reward
  , _result    :: First Result
  }

-- # EarlyGameDataSemigroup
instance Semigroup GameData where
  g1 <> g2 = GameData
    { _waitingOn =
        M.unionWith (<>)  -- ! 1
          (_waitingOn g1)
          (_waitingOn g2)
    , _rewards = _rewards g1 <> _rewards g2
    , _result  = _result  g1 <> _result  g2
    }

-- # EarlyGameDataMonoid
instance Monoid GameData where
  mempty = GameData mempty mempty mempty

-}

instance Show (GameData s) where
  show (GameData e r (First res)) = mconcat
    [ "Game { waitingFor = "
    , show $ M.keys e
    , ", result = "
    , show res
    , ", rewards = "
    , show r
    , " }"
    ]

instance Semigroup (GameData s) where
  g1 <> g2 = GameData
    { _waitingOn =
        M.unionWith (<>)
          (_waitingOn g1)
          (_waitingOn g2)
    , _rewards = _rewards g1 <> _rewards g2
    , _result  = _result  g1 <> _result  g2
    }
  {-# INLINABLE (<>) #-}

instance Monoid (GameData s) where
  mempty = GameData mempty mempty mempty
  {-# INLINABLE mempty #-}

win :: Game
win = Game $ \w _ -> w
{-# INLINABLE win #-}

lose :: Game
lose = Game $ \_ l -> l
{-# INLINABLE lose #-}

reward :: Reward -> Game
reward r = rewardThen r win
{-# INLINABLE reward #-}

{-

-- # earlyRewardThen
rewardThen :: Reward -> Game -> Game
rewardThen r (Game g) = Game $ \w l ->
  mempty { _rewards = S.singleton r }
    <> g w l

-}

rewardThen :: Reward -> Game -> Game
rewardThen r (Game g) = Game $ \w l -> do
  gd <- g w l
  pure $ mempty { _rewards = S.singleton r } <> gd
{-# INLINABLE rewardThen #-}

eitherG :: Game -> Game -> Game
eitherG (Game g1) (Game g2) = Game $ \w l -> do
  filled  <- newSTRef False
  remaining_losses  <- newSTRef @Int 2
  let runLose = do
        modifySTRef' remaining_losses $ subtract 1
        readSTRef remaining_losses >>= \case
          0 -> _oneshot filled l
          _ -> pure mempty
  liftA2 (<>)
    (g1 (_oneshot filled w) runLose)
    (g2 (_oneshot filled w) runLose)
{-# INLINABLE eitherG #-}

_oneshot :: Monoid a => STRef s Bool -> ST s a -> ST s a
_oneshot ref v =
  readSTRef ref >>= \case
    True  -> pure mempty
    False -> do
      writeSTRef ref True
      v
{-# INLINABLE _oneshot #-}

andThen :: Game -> Game -> Game
andThen (Game g1) (Game g2) = Game $ \w l ->
  g1 (g2 w l) l
{-# INLINABLE andThen #-}

both :: Game -> Game -> Game
both (Game g1) (Game g2) = Game $ \w l -> do
  filled  <- newSTRef False
  remaining_wins  <- newSTRef @Int 2  -- ! 1
  let runWin = do  -- ! 2
        modifySTRef' remaining_wins $ subtract 1
        readSTRef remaining_wins >>= \case
          0 -> _oneshot filled w
          _ -> pure mempty
  liftA2 (<>)
    (g1 runWin $ _oneshot filled l)  -- ! 3
    (g2 runWin $ _oneshot filled l)
{-# INLINABLE both #-}


{-

-- # earlyGate
gate :: EventFilter -> Game -> Game
gate ef (Game g) = Game $ \w l ->
  mempty { _waitingOn = M.singleton ef $ g w l }

-}

gate :: EventFilter -> Game -> Game
gate ef (Game g) = Game $ \w l -> do
  pure $ mempty { _waitingOn = M.singleton ef $ g w l }
{-# INLINABLE gate #-}

subgame :: Game -> Game -> Game -> Game
subgame (Game g) (Game gw) (Game gl) = Game $ \w l ->
  g (gw w l) (gl w l)
{-# INLINABLE subgame #-}

comeback :: Game -> Game
comeback (Game g) = Game $ \w l -> g l w
{-# INLINABLE comeback #-}

choose :: [(EventFilter, Game)] -> Game
choose cs = Game $ \w l -> do
  filled <- newSTRef False
  let makeGame (Game g) = _oneshot filled $ g w l
  pure $ mempty
    { _waitingOn = M.fromList $ fmap (fmap makeGame) cs
    }
{-# INLINABLE choose #-}

race :: Game -> Game -> Game
race (Game g1) (Game g2) = Game $ \w l -> do
  filled <- newSTRef False  -- ! 1
  liftA2 (<>)
    (g1 (_oneshot filled w)  -- ! 2
        (_oneshot filled l))
    (g2 (_oneshot filled w)
        (_oneshot filled l))
{-# INLINABLE race #-}


bottom :: Game
bottom = Game $ \_ _ -> mempty
{-# INLINABLE bottom #-}


giveVictory :: ST s (GameData s)
giveVictory = pure $ mempty { _result = pure Victory }
{-# INLINABLE giveVictory #-}

giveDefeat :: ST s (GameData s)
giveDefeat = pure $ mempty { _result = pure Defeat }
{-# INLINABLE giveDefeat #-}

runGame :: [Event] -> Game -> (Set Reward, Maybe Result)
runGame evs (Game g) = runST $ do
  gd' <-
    _pumpGame evs =<<
      g giveVictory giveDefeat  -- ! 1
  pure (_rewards gd', getFirst $ _result gd')


_pumpGame :: [Event] -> GameData s -> ST s (GameData s)
_pumpGame [] gd = pure gd
_pumpGame _ gd
  | First (Just _) <- _result gd  -- ! 1
  = pure gd
_pumpGame (e : es) gd = _pumpGame es =<< _stepGame e gd


_stepGame :: Event -> GameData s -> ST s (GameData s)
_stepGame e gd = do
  let efs = M.assocs $ _waitingOn gd  -- ! 1
  (endo, gds) <-
    flip foldMapM efs $ \(ef, res) ->  -- ! 2
      case matches ef e of  -- ! 3
        True -> do
          gd' <- res  -- ! 4
          pure (Endo $ M.delete ef, gd')  -- ! 5
        False -> mempty
  pure $
    gd { _waitingOn =
           appEndo endo $ _waitingOn gd   -- ! 6
       } <> gds  -- ! 7

foldMapM
    :: (Monoid m, Applicative f, Traversable t)
    => (a -> f m)
    -> t a
    -> f m
foldMapM f = fmap fold . traverse f



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
  = Defeat
  | Victory
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
        , (5, choose <$> decayArbitrary 5)
        , (2, comeback  <$> arbitrary)
        , (1, pure bottom)
        , (5, gate <$> arbitrary <*> arbitrary)
        ]


-- # ShowGame
instance Show Game where
  show (Game g) =
    runST $ fmap show $ g giveVictory giveDefeat

-- # ObserveGame
instance
    Observe [Event] (Set Reward, Maybe Result) Game
    where
  observe = runGame

decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary

sig_games_core :: Sig
sig_games_core = signature
  [ con        "win" win
  , con       "lose" lose
  , con    "subgame" subgame
  , con    "eitherG" eitherG
  , con       "both" both
  , con       "race" race
  , con  "choose" choose
  , con "rewardThen" rewardThen
  ]

------------------------------------------------------------------------------
--                         extensions
------------------------------------------------------------------------------

sig_games_ext :: Sig
sig_games_ext = signature
  [ con "comeback" comeback
  , con   "bottom" bottom
  , con     "gate" gate
  , con  "andThen" andThen
  , con   "reward" reward
  ]


bingo :: [[Game]] -> Reward -> Game
bingo squares r
  = let subgames = squares
                ++ transpose squares  -- ! 1
        allOf = foldr both    win
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


------------------------------------------------------------------------------
--                         observations
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
--                           tests
------------------------------------------------------------------------------

main :: IO ()
main = traverse_ (quickCheckWith stdArgs {maxSuccess=1000} . uncurry counterexample) quickspec_laws'


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
  , ( "both lose (choose xs) = lose"
    , property $
        \ (xs :: [(EventFilter, Game)]) ->
            both lose (choose xs) =~= lose)
  , ( "race (comeback g) lose = comeback (race g win)"
    , property $
        \ (g :: Game) ->
            race (comeback g) lose =~= comeback (race g win))
  , ( "race (choose xs) lose = lose"
    , property $
        \ (xs :: [(EventFilter, Game)]) ->
            race (choose xs) lose =~= lose)
  , ( "race (choose xs) win = win"
    , property $
        \ (xs :: [(EventFilter, Game)]) ->
            race (choose xs) win =~= win)
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

