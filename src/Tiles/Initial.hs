{-# LANGUAGE BangPatterns                         #-}
{-# LANGUAGE DeriveFunctor                        #-}
{-# LANGUAGE DerivingVia                          #-}
{-# LANGUAGE FlexibleContexts                     #-}
{-# LANGUAGE FlexibleInstances                    #-}
{-# LANGUAGE GADTs                                #-}
{-# LANGUAGE MultiParamTypeClasses                #-}
{-# LANGUAGE PatternSynonyms                      #-}
{-# LANGUAGE QuantifiedConstraints                #-}
{-# LANGUAGE TemplateHaskell                      #-}
{-# LANGUAGE TypeSynonymInstances                 #-}
{-# LANGUAGE UndecidableInstances                 #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tiles.Initial
  ( -- * Observations
    rasterize
  , rasterize'
  , toPNG

    -- * Generic constructors
  , empty
  , cw
  , ccw
  , beside
  , cols
  , above
  , rows
  , flipH
  , flipV
  , quad
  , swirl

    -- * Color constructors
  , behind
  , color

    -- * Special color constructors
  , haskell
  , sandy
  , spj

    -- * Color operations
  , rgba
  , invert
  , mask

    -- * QuickSpec signatures
  , sig

    -- * Types
  , Tile
  , Color
  , pattern Color
  ) where

import Codec.Picture.Png
import Codec.Picture.Types
import Control.Applicative hiding (empty)
import Data.Coerce
import Data.FileEmbed
import Data.Functor.Compose
import Data.List (transpose)
import Data.Word
import QuickSpec
import Test.QuickCheck hiding (label, sample)

------------------------------------------------------------------------------

type Color = PixelRGBA8

instance Semigroup Color where
  (<>) = _over

instance Monoid Color where
  mempty = rgba 0 0 0 0

color :: Double -> Double -> Double -> Double -> Tile Color
color r g b a = pure $ rgba r g b a

rgba :: Double -> Double -> Double -> Double -> Color
rgba r g b a =
  PixelRGBA8
    (bounded r)
    (bounded g)
    (bounded b)
    (bounded a)
  where
    bounded :: Double -> Word8
    bounded x = round $ x * fromIntegral (maxBound @Word8)

pattern Color :: Double -> Double -> Double -> Double -> Color
pattern Color r g b a <-
  PixelRGBA8
    (fromIntegral -> (/255) -> r)
    (fromIntegral -> (/255) -> g)
    (fromIntegral -> (/255) -> b)
    (fromIntegral -> (/255) -> a)
  where
    Color = rgba
{-# COMPLETE Color #-}

invert :: Color -> Color
invert (Color r g b a) = Color (1 - r) (1 - g) (1 - b) a

-- # SemigroupTile
instance Semigroup a => Semigroup (Tile a) where
  (<>) = liftA2 (<>)

-- # MonoidTile
instance Monoid a => Monoid (Tile a) where
  mempty = pure mempty


data Tile a
  = Cw (Tile a)
  | FlipH (Tile a)
  | Above [Tile a]
  | Pure a
  | forall b. Ap (Tile (b -> a)) (Tile b)

instance Functor Tile where
  fmap f = (pure f <*>)

instance Applicative Tile where
  pure = Pure
  (<*>) = Ap

instance Show a => Show (Tile a) where
  show (Cw t) = "cw (" ++ show t ++ ")"
  show (FlipH t) = "flipH (" ++ show t ++ ")"
  show (Above [a,b]) = "above (" ++ show a ++ ") (" ++ show b ++ ")"
  show (Above as) = "rows " ++ show as
  show (Pure a) = "pure (" ++ show a ++ ")"
  show (Ap _ _) = "ap _ _"

-- # ArbitraryTile
instance (CoArbitrary a, Arbitrary a)
      => Arbitrary (Tile a) where
  arbitrary = sized $ \n ->  -- ! 1
    case n <= 1 of
      True -> pure <$> arbitrary  -- ! 2
      False -> frequency  -- ! 3
        [ (3,) $ pure <$> arbitrary  -- ! 4
        , (9,) $ beside <$> decayArbitrary 2  -- ! 5
                        <*> decayArbitrary 2
        , (9,) $ above <$> decayArbitrary 2
                       <*> decayArbitrary 2
        , (2,) $ cw <$> arbitrary
        , (2,) $ ccw <$> arbitrary
        , (4,) $ flipV <$> arbitrary
        , (4,) $ flipH <$> arbitrary
        , (6,) $ swirl <$> decayArbitrary 4
        , (3,) $ quad <$> decayArbitrary 4
                      <*> decayArbitrary 4
                      <*> decayArbitrary 4
                      <*> decayArbitrary 4
        , (2,) $ (<*>)
              <$> decayArbitrary @(Tile (a -> a)) 2
              <*> decayArbitrary 2
        ]

  shrink (Cw t)      = t : (cw <$> shrink t)
  shrink (FlipH t)   = t : (flipH <$> shrink t)
  shrink (Above ts) = ts ++ filter valid (fmap Above (shrink ts))
  shrink (Pure a)    = pure <$> shrink a
  shrink (Ap _ _)    = []

valid :: Tile a -> Bool
valid (Above []) = False
valid _ = True

instance Observe () Color Color

-- # ObserveTile
instance Observe test outcome [[a]]
      => Observe
            (Small Int, Small Int, test)
            outcome
            (Tile a) where
  observe (Small w, Small h, x) t
    = observe x (rasterize (max 1 w) (max 1 h) t)

decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary

instance CoArbitrary PixelRGBA8 where
  coarbitrary (Color r g b a) = coarbitrary (r, g, b, a)

instance Arbitrary PixelRGBA8 where
  arbitrary = do
    a <- choose (0, 255)
    case a == 0 of
      True  -> pure mempty
      False -> PixelRGBA8 <$> choose (0,255) <*> choose (0,255) <*> choose (0,255) <*> pure a

cw :: Tile a -> Tile a
cw (Cw (Cw (Cw x))) = x
cw x = Cw x

ccw :: Tile a -> Tile a
ccw (Cw x) = x
ccw x = cw (cw (cw x))

_fromImage :: Image PixelRGBA8 -> Tile Color
_fromImage img@(Image w h _) = rows $ do
  y <- [0 .. h - 1]
  pure $ cols $ do
    x <- [0 .. w - 1]
    pure $ pure $ pixelAt img x y

beside :: Tile a -> Tile a -> Tile a
beside t1 t2 = ccw (above (cw t1) (cw t2))

above :: Tile a -> Tile a -> Tile a
above t1 t2 = Above [t1, t2]

behind :: Monoid a => Tile a -> Tile a -> Tile a
behind = flip (liftA2 (<>))

flipH :: Tile a -> Tile a
flipH (FlipH t) = t
flipH t = FlipH t

flipV :: Tile a -> Tile a
flipV = ccw . flipH . cw

empty :: Monoid a => Tile a
empty = pure mempty

rows :: Monoid a => [Tile a] -> Tile a
rows [] = pure mempty
rows [x] = x
rows ts = Above ts

cols :: Monoid a => [Tile a] -> Tile a
cols [] = pure mempty
cols [x] = x
cols ts = ccw . rows $ fmap cw ts


quad :: Tile a -> Tile a -> Tile a -> Tile a -> Tile a
quad t1 t2 t3 t4 = (t1 `beside` t2) `above` (t3 `beside` t4)

swirl :: Tile a -> Tile a
swirl t = quad t (cw t) (ccw t) $ cw $ cw t

_over :: Color -> Color -> Color
_over (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
  let aa = norm a1
      ab = norm a2
      a' = aa + ab * (1 - aa)
      norm :: Word8 -> Double
      norm x = fromIntegral x / 255
      unnorm :: Double -> Word8
      unnorm x = round $ x * 255
      f :: Word8 -> Word8 -> Word8
      f a b = unnorm $ (norm a * aa + norm b * ab * (1 - aa)) / a'
   in
  PixelRGBA8 (f r1 r2) (f g1 g2) (f b1 b2) (unnorm a')

mask :: Color -> Color -> Color
mask (PixelRGBA8 _ _ _ a) (PixelRGBA8 r g b _) = PixelRGBA8 r g b a


----------------------------------------------------------------------------------

toPNG :: Int -> Int -> Tile Color -> Image PixelRGBA8
toPNG w h t = generateImage f w h
  where
    img = rasterize w h t
    f x y = img !! y !! x

----------------------------------------------------------------------------------


haskell :: Tile Color
haskell = do
  let Right (ImageRGBA8 img) = decodePng $(embedFile "static/haskell.png")
   in _fromImage img
{-# NOINLINE haskell #-}

sandy :: Tile Color
sandy =
  let Right (ImageRGBA8 img) = decodePng $(embedFile "static/sandy.png")
   in _fromImage img
{-# NOINLINE sandy #-}

spj :: Tile Color
spj = do
  let Right (ImageRGBA8 img) = decodePng $(embedFile "static/spj.png")
   in _fromImage img
{-# NOINLINE spj #-}



------------------------------------------------------------------------------
-- | Rasterize a 'Tile' down into a row-major representation of its constituent
-- "pixels".
rasterize :: Int -> Int -> Tile a -> [[a]]
rasterize w h (Pure a) = replicate h $ replicate w a
rasterize w h (Ap f a) =
  coerce (rasterize' w h f <*> rasterize' w h a)
rasterize w h (FlipH t) = fmap reverse $ rasterize w h t
rasterize w h (Cw t) = rotate2d $ rasterize h w t
  where
    rotate2d = fmap reverse . transpose
rasterize w h (Above [t]) = rasterize w h t
rasterize _ _ (Above []) = error "you broke the invariant!"
rasterize w h (Above z@(t:ts))
  | h >= length z =
      let h' = div h (length z)
       in rasterize w h' t <>
            rasterize w (h - h') (Above ts)
  | otherwise =
      let zspan = fromIntegral @_ @Double (length z) / fromIntegral h
       in rasterize w h $ Above $ do
            y <- [0..h-1]
            pure $ ts !! floor (fromIntegral y * zspan)


------------------------------------------------------------------------------
-- | Like 'rasterize'', but with a type more convenient for showing off the
-- applicative homomorphism.
rasterize'
    :: Int  -- ^ resulting width
    -> Int  -- ^ resulting heigeht
    -> Tile a
    -> Compose ZipList ZipList a  -- ^ the resulting "pixels" in row-major order
rasterize' w h t = coerce $ rasterize w h t

sig :: Sig
sig = sig_bg <> sig_cons <> sig_types


sig_bg :: Sig
sig_bg = background
  [ con "<>"     $ liftC @(Monoid A) $ (<>)   @A
  , con "mempty" $ liftC @(Monoid A) $ mempty @A
  ]

sig_cons :: Sig
sig_cons = signature
  [ con "cw"     $ cw     @A  -- ! 1
  , con "ccw"    $ ccw    @A
  , con "beside" $ beside @A
  , con "above"  $ above  @A
  , con "flipV"  $ flipV  @A
  , con "flipH"  $ flipH  @A
  , con "pure"   $ pure   @Tile @A
  , con "<*>"    $ (<*>)  @Tile @A @B
  , con "quad"   $ quad   @A
  , con "swirl"  $ swirl  @A
  , con "behind" $ liftC @(Monoid A) $ behind @A  -- ! 2
  , con "empty"  $ liftC @(Monoid A) $ empty  @A
  ]


sig_types :: forall m. (m ~ [Word8]) => Sig
sig_types = signature
  [ mono        @m  -- ! 1
  , monoObserve @(Tile m)  -- ! 2
  , monoObserve @(Tile (m -> m))
  , instanceOf  @(Monoid m)  -- ! 3
  , instanceOf  @(Monoid (Tile m))
  , vars ["t"]  $ Proxy @(Tile A)  -- ! 4
  , vars ["tf"] $ Proxy @(Tile (A -> B))
  , defaultTo $ Proxy @m  -- ! 5
  , withMaxTermSize 5
  ]


