{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wall              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ADD.Tiles.Functor
  ( -- * Tiles and their observations
    Tile ()
  , rasterize
  , rasterize'
  , toImage

    -- * Tile constructors
  , empty
  , color
  , cw
  , ccw
  , flipH
  , flipV
  , beside
  , rows
  , above
  , cols
  , behind
  , quad
  , quads
  , swirl
  , nona

    -- * Special tiles
  , haskell
  , sandy

    -- * Colors and their observations
  , Color
  , redChannel
  , greenChannel
  , blueChannel
  , alphaChannel

    -- * Color constructors
  , pattern Color
  , invert
  , mask
  , over
  ) where

import Codec.Picture.Png
import Codec.Picture.Types
import Control.Applicative hiding (empty)
import Data.Coerce
import Data.FileEmbed
import Data.Functor.Compose
import Data.Word
import Test.QuickCheck hiding (label)


------------------------------------------------------------------------------

type Color = PixelRGBA8

instance Semigroup Color where
  (<>) = over

instance Monoid Color where
  mempty = Color 0 0 0 0

color :: Double -> Double -> Double -> Double -> Tile Color
color r g b a = pure $ _rgba r g b a

------------------------------------------------------------------------------
-- | Extract the red channel from a 'Color'.
redChannel :: Color -> Double
redChannel (Color r _ _ _) = r

------------------------------------------------------------------------------
-- | Extract the green channel from a 'Color'.
greenChannel :: Color -> Double
greenChannel (Color _ g _ _) = g

------------------------------------------------------------------------------
-- | Extract the blue channel from a 'Color'.
blueChannel :: Color -> Double
blueChannel (Color _ _ b _) = b

------------------------------------------------------------------------------
-- | Extract the alpha channel from a 'Color'.
alphaChannel :: Color -> Double
alphaChannel (Color _ _ _ a) = a

------------------------------------------------------------------------------
-- | Inverts a 'Color' by negating each of its color channels, but leaving the
-- alpha alone.
invert :: Color -> Color
invert (Color r g b a) = Color (1 - r) (1 - g) (1 - b) a


_rgba :: Double -> Double -> Double -> Double -> Color
_rgba r g b a =
  PixelRGBA8
    (bounded r)
    (bounded g)
    (bounded b)
    (bounded a)
  where
    bounded :: Double -> Word8
    bounded x = round $ x * fromIntegral (maxBound @Word8)

------------------------------------------------------------------------------
-- |
pattern Color :: Double -> Double -> Double -> Double -> Color
pattern Color r g b a <-
  PixelRGBA8
    (fromIntegral -> (/255) -> r)
    (fromIntegral -> (/255) -> g)
    (fromIntegral -> (/255) -> b)
    (fromIntegral -> (/255) -> a)
  where
    Color = _rgba
{-# COMPLETE Color #-}

instance Semigroup a => Semigroup (Tile a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Tile a) where
  mempty = pure mempty


newtype Tile a = Tile
  { runTile :: Double -> Double -> a
  }
  deriving stock (Functor)
  deriving Applicative via (Compose ((->) Double) ((->) Double))

instance Show (Tile t) where
  show _ = "<tile>"

instance Arbitrary a => Arbitrary (Tile a) where
  arbitrary = Tile <$> arbitrary

instance CoArbitrary PixelRGBA8 where
  coarbitrary (Color r g b a) = coarbitrary (r, g, b, a)

instance Arbitrary PixelRGBA8 where
  arbitrary = PixelRGBA8 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Monad Tile where
  Tile ma >>= f = Tile $ \x y -> runTile (f (ma x y)) x y

------------------------------------------------------------------------------
-- | Rotate a 'Tile' clockwise.
cw :: Tile a -> Tile a
cw (Tile f) = Tile $ \x y -> f y (1 - x)


------------------------------------------------------------------------------
-- | Rotate a 'Tile' counterclockwise.
ccw :: Tile a -> Tile a
ccw (Tile f) = Tile $ \x y -> f (1 - y) x

_fromImage :: Image PixelRGBA8 -> Tile Color
_fromImage img@(Image w h _) = Tile $ \x y ->
  pixelAt
    img
    (max 0 (min (w - 1) (floor $ x * fromIntegral w)))
    (max 0 (min (h - 1) (floor $ y * fromIntegral h)))


------------------------------------------------------------------------------
-- | Place the first 'Tile' to the left of the second. Each 'Tile' will receive
-- half of the available width, but keep their full height.
beside :: Tile a -> Tile a -> Tile a
beside (Tile a) (Tile b) = Tile $ \x y ->
  case x >= 0.5 of
    False -> a (2 * x) y
    True  -> b (2 * (x - 0.5)) y


------------------------------------------------------------------------------
-- | Place the first 'Tile' above the second. Each 'Tile' will receive half of
-- the available height, but keep their full width.
above :: Tile a -> Tile a -> Tile a
above (Tile a) (Tile b) = Tile $ \x y ->
  case y >= 0.5 of
    False -> a x (2 * y)
    True  -> b x (2 * (y - 0.5))


------------------------------------------------------------------------------
-- | Place the first 'Tile' behind the second. The result of this operation is
-- for transparent or semi-transparent pixels in the second argument to be
-- blended via 'over' with those in the first.
behind :: Tile Color -> Tile Color -> Tile Color
behind = flip (liftA2 over)


------------------------------------------------------------------------------
-- | Mirror a 'Tile' horizontally.
flipH :: Tile a -> Tile a
flipH (Tile t) = Tile $ \x y ->
  t (1 - x) y


------------------------------------------------------------------------------
-- | Mirror a 'Tile' vertically.
flipV :: Tile a -> Tile a
flipV (Tile t) = Tile $ \x y ->
  t x (1 - y)


------------------------------------------------------------------------------
-- | The empty, fully transparent 'Tile'.
empty :: Tile Color
empty = pure mempty


------------------------------------------------------------------------------
-- | Like 'above', but repeated. Every element in the list will take up
-- a proportional height of the resulting 'Tile'.
rows :: Monoid a => [Tile a] -> Tile a
rows [] = mempty
rows ts =
  let n = length ts
   in Tile $ \x y ->
        let i = floor $ fromIntegral n * y
         in runTile (ts !! i) x y


------------------------------------------------------------------------------
-- | Like 'beside', but repeated. Every element in the list will take up
-- a proportional width of the resulting 'Tile'.
cols :: Monoid a => [Tile a] -> Tile a
cols [] = mempty
cols ts =
  let n = length ts
   in Tile $ \x y ->
        let i = floor $ fromIntegral n * x
         in runTile (ts !! i) x y


------------------------------------------------------------------------------
-- | Place four 'Tile's in the four quadrants. The first argument is the
-- top-left; the second is the top-right; third: bottom left; fourth: bottom
-- right.
quad :: Tile a -> Tile a -> Tile a -> Tile a -> Tile a
quad a b c d = (a `beside` b) `above` (c `beside` d)

------------------------------------------------------------------------------
-- | Like `quad`, but constructs a 'Tile' of endomorphisms. The given function
-- is called one more time for each quadrant, starting clockwise from the
-- top-left.
quads :: (a -> a) -> Tile (a -> a)
quads f =
  quad
    (pure id)
    (pure f)
    (pure $ f . f . f)
    (pure $ f . f)


------------------------------------------------------------------------------
-- | A 'quad' where the given 'Tile' is rotated via 'cw' once more per
-- quadrant.
swirl :: Tile a -> Tile a
swirl t = quad t (cw t) (ccw t) $ cw $ cw t


------------------------------------------------------------------------------
-- | Puts a frame around a 'Tile'. The first argument is the straight-edge
-- border for the top of the frame. The second argument should be for the
-- top-right corner. The third argument is the 'Tile' that should be framed.
nona :: Monoid a => Tile a -> Tile a -> Tile a -> Tile a
nona t tr c =
  rows [ cols [ ccw tr,      t,         tr    ]
       , cols [ ccw t,       c,         cw t  ]
       , cols [ cw (cw tr),  cw $ cw t, cw tr ]
       ]

------------------------------------------------------------------------------
-- | Blends a 'Color' using standard alpha compositing.
over :: Color -> Color -> Color
over (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
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


------------------------------------------------------------------------------
-- | Copy the alpha channel from the first 'Color' and the color channels from
-- the second 'Color'.
mask :: Color -> Color -> Color
mask (PixelRGBA8 _ _ _ a) (PixelRGBA8 r g b _) = PixelRGBA8 r g b a


--------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Like 'rasterize', but into a format that can be directly saved to disk as
-- an image.
toImage
    :: Int  -- ^ resulting width
    -> Int  -- ^ resulting height
    -> Tile Color
    -> Image PixelRGBA8
toImage w h (Tile t) = generateImage f w h
  where
    coord :: Int -> Int -> Double
    coord dx x = fromIntegral dx / fromIntegral x
    f :: Int -> Int -> PixelRGBA8
    f x y = t (coord x w) (coord y h)


------------------------------------------------------------------------------
-- | The Haskell logo.
haskell :: Tile Color
haskell =
  let Right (ImageRGBA8 img) = decodePng $(embedFile "static/haskell.png")
   in _fromImage img

------------------------------------------------------------------------------
-- | Sandy.
sandy :: Tile Color
sandy =
  let Right (ImageRGBA8 img) = decodePng $(embedFile "static/sandy.png")
   in _fromImage img


------------------------------------------------------------------------------
-- | Rasterize a 'Tile' down into a row-major representation of its constituent
-- "pixels". For a version that emits a list of lists directly, see 'rasterize''.
rasterize
    :: forall a
     . Int  -- ^ resulting width
    -> Int  -- ^ resulting heigeht
    -> Tile a
    -> Compose ZipList ZipList a  -- ^ the resulting "pixels" in row-major order
rasterize w h (Tile t) = coerce $ do
  y <- [0 .. (h - 1)]
  pure $ do
    x <- [0 .. (w - 1)]
    pure $ f x y

  where
    coord :: Int -> Int -> Double
    coord dx x = fromIntegral dx / fromIntegral x

    f :: Int -> Int -> a
    f x y = t (coord x w) (coord y h)

------------------------------------------------------------------------------
-- | Like 'rasterize', but with a more convenient output type.
rasterize'
    :: Int  -- ^ resulting width
    -> Int  -- ^ resulting heigeht
    -> Tile a
    -> [[a]]  -- ^ the resulting "pixels" in row-major order
rasterize' w h t = coerce $ rasterize w h t

