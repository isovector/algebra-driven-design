{-# LANGUAGE DeriveDataTypeable                   #-}
{-# LANGUAGE DeriveFunctor                        #-}
{-# LANGUAGE DeriveGeneric                        #-}
{-# LANGUAGE FlexibleInstances                    #-}
{-# LANGUAGE MultiParamTypeClasses                #-}
{-# LANGUAGE NumDecimals                          #-}
{-# LANGUAGE PatternSynonyms                      #-}
{-# LANGUAGE ScopedTypeVariables                  #-}
{-# LANGUAGE TupleSections                        #-}
{-# LANGUAGE TypeApplications                     #-}
{-# LANGUAGE TypeSynonymInstances                 #-}
{-# OPTIONS_GHC -Wall                             #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans                 #-}
{-# OPTIONS_GHC -fno-warn-unused-imports          #-}

module Lib where

import Data.Foldable
import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Data.Bool
import           Data.Data
import           Data.Either
import           Data.List
import           Data.Ord
import           Data.Tuple
import           GHC.Generics
import           QuickSpec
import           System.Timeout
import qualified Test.QuickCheck as QC
import           Test.QuickCheck hiding (oneof, choose, Negative (..))


data Regex a
  = Char a
  | Empty
  | AnyChar
  | Fail
  | Cat (Regex a) (Regex a)
  | Choose (Regex a) (Regex a)
  | Optional (Regex a)
  | Star (Regex a)
  | Lookahead (Regex a)
  | Both (Regex a) (Regex a)
  | Negative (Regex a)
  deriving (Eq, Ord, Show, Data, Generic)

both :: Regex a -> Regex a -> Regex a
both = Both

negative :: Regex a -> Regex a
negative = Negative

lookahead :: Regex a -> Regex a
lookahead = Lookahead

char :: a -> Regex a
char = Char

anyChar :: Regex a
anyChar = AnyChar

emptyStr :: Regex a
emptyStr = Empty

failMatch :: Regex a
failMatch = Fail

cat :: Regex a -> Regex a -> Regex a
cat (Cat x y) z = Cat x (cat y z)
cat x y = Cat x y

cats :: [Regex a] -> Regex a
cats = foldr cat emptyStr

choose :: Eq a => Regex a -> Regex a -> Regex a
choose (Cat x y) (Cat x' z)
  | x == x' = cat x (choose y z)
choose Fail x = x
choose x Fail = x
choose x y
  | x == y    = x
  | otherwise = Choose x y

oneof :: Eq a => [Regex a] -> Regex a
oneof = foldr choose failMatch

optional :: Regex a -> Regex a
optional (Star x) = Star x
optional (Optional x) = Optional x
optional Empty = emptyStr
optional x = Optional x

star :: Eq a => Regex a -> Regex a
star (Star x) = Star x
star (Optional x) = star x
star (Choose x (Star y)) = star (choose x y)
star (Choose (Star x) y) = star (choose x y)
star Fail = emptyStr
star Empty = optional Empty
star x = Star x

digit :: Regex Char
digit = oneof (map char ['0' .. '9'])

number :: Regex Char
number =
  cats
    [ digit
    , star digit
    , optional (
        cats
          [ char '.'
          , digit
          , star digit
          , optional (char 'f')
          ]
      )
    ]

consume :: (Show a, Eq a) => Regex a -> [a] -> [[a]]
consume (Both r1 r2) s = do
  m1 <- consume r1 s
  m2 <- consume r2 s
  let [small, large] = sortBy (comparing length) [m1, m2]
  guard $ small `isSuffixOf` large
  pure small
consume (Negative r) s =
  case null $ consume r s of
    True  -> [s]
    False -> []
consume (Lookahead r) s = s <$ consume r s
consume Empty s             = [s]
consume AnyChar (_ : cs) = [cs]
consume AnyChar [] = []
consume (Char c) (c' : cs) = bool [] [cs] $ c == c'
consume (Char _) [] = []
consume Fail _              = []
consume (Cat r1 r2) str     = do
  str' <- consume r1 str
  consume r2 str'
consume (Choose r1 r2) str  = consume r1 str ++ consume r2 str
consume (Optional r) str    = str : consume r str
consume (Star _) []      = [[]]
consume s@(Star r) str      = str : do
  -- traceM $ show str
  str' <- consume r str
  if str /= str'
     then consume s str'
     else []

match :: (Show a, Eq a) => Regex a -> [a] -> Bool
match r s = not . null $ consume r s

------------------------------------------------------------------------------
-- | quickcheck


instance (Eq a, Arbitrary a) => Arbitrary (Regex a) where
  arbitrary = subtermFrequency
    [ (Cheap, char <$> arbitrary)
    , (Cheap, pure emptyStr)
    , (Cheap, pure failMatch)
    , (Cheap, pure anyChar)
    , (Expensive 2, cat      <$> smallerArbitrary 2 <*> smallerArbitrary 2)
    , (Expensive 2, choose   <$> smallerArbitrary 2 <*> smallerArbitrary 2)
    , (Expensive 4, star     <$> smallerArbitrary 2)
    , (Expensive 3, optional <$> scale (subtract 1) arbitrary)
    , (Expensive 2, lookahead <$> scale (subtract 1) arbitrary)
    , (Expensive 2, negative <$> scale (subtract 1) arbitrary)
    , (Expensive 4, both     <$> smallerArbitrary 2 <*> smallerArbitrary 2)
    ]
  shrink AnyChar = []
  shrink a = AnyChar : genericShrink a

smallerArbitrary :: Arbitrary a => Int -> Gen a
smallerArbitrary n = smallerGen n arbitrary

smallerGen :: Int -> Gen a -> Gen a
smallerGen n = scale (`div` n)

data Freq
  = Cheap
  | Expensive Int

splitFreq :: x -> Freq -> Either x Int
splitFreq x Cheap = Left x
splitFreq _ (Expensive x) = Right x

subtermFrequency :: [(Freq, Gen a)] -> Gen a
subtermFrequency qs =
  let (cheaps, exps) =
        partitionEithers
          $ fmap (\x -> sequence . swap $ first (splitFreq $ snd x) x) qs
   in QC.oneof
        [ QC.oneof cheaps
        , sized $ \size ->
            QC.frequency $
              (fmap (first (div size)) $ fmap swap exps) ++ fmap (1,) cheaps
        ]

------------------------------------------------------------------------------
-- | quickspec

data Timeout a = Timeout | Got a
  deriving (Eq, Ord, Show, Functor)

quickly :: (a -> b -> c) -> a -> b -> IO (Timeout c)
quickly c a b =
    fmap (maybe Timeout Got)
    $ timeout 1
    $ evaluate
    $ a `seq`
      b `seq`
      c a b

instance (Show a, Arbitrary a, Ord a) => Observe [a] [[a]] (Regex a) where
  observe s r = sort . nub $ consume r s


sig :: Sig
sig = series
  [ [ con "cat"       $ cat       @Bool3
    , con "char"      $ char      @Bool3
    -- , con "choose"    $ choose    @Bool3
    -- , con "star"      $ star      @Bool3
    , con "failMatch" $ failMatch @Bool3
    -- , con "emptyStr"  $ emptyStr  @Bool3
    -- , con "negative"  $ negative @Bool3
    -- , con "lookahead" $ lookahead @Bool3
    -- , con "optional"  $ optional  @Bool3

    , withMaxTermSize 7
    , withMaxTests 1000
    , withMaxTestSize 10

    , monoTypeObserve $ Proxy @(Regex Bool3)
    , monoType $ Proxy @[Bool3]
    , monoType $ Proxy @Bool3
    , defaultTo $ Proxy @Bool3
    ]

  , [ con "both"  $ both @Bool3
    , predicate "/=" $  (/=) @Bool3
    -- , withLinearity Linear
    -- , withPrintStyle ForQuickCheck
    ]

  ]

data Bool3 = True3 | False3 | Tri3 deriving (Eq, Ord, Show, Data, Generic)

instance Arbitrary Bool3 where
  arbitrary = elements [True3, False3, Tri3]
  shrink = genericShrink


main :: IO ()
main = quickSpec sig
-- main = traverse_ (quickCheck . uncurry counterexample) quickspec_tests

quickspec_tests :: [(String, Property)]
quickspec_tests =
  [ ( "x /= y => both (cat (char x) z) (cat (char y) z) = failMatch"
    , property $
        \ (x :: Bool3) (y :: Bool3) (z :: Regex Bool3) ->
            x /= y ==> both (cat (char x) z) (cat (char y) z) =~= failMatch)
  ]

