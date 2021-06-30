{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tiles.Efficient where


import qualified Codec.Picture.Metadata as MD
import           Codec.Picture.Png
import           Codec.Picture.Types
import           Control.Applicative hiding (empty)
import           Data.Bool
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lb
import           Data.Char
import           Data.Functor.Compose
import qualified Data.Hashable as H
import           Data.List (group)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Word
import           QuickSpec
import           System.IO.Unsafe
import           Test.QuickCheck hiding (label, sample)

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

instance Semigroup a => Semigroup (Tile a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Tile a) where
  mempty = pure mempty


newtype Tile a = Tile
  { sample :: Double -> Double -> a
  }
  deriving (Functor, Applicative)
    via (Compose ((->) Double) ((->) Double))

instance Show (Tile t) where
  show _ = "<tile>"

-- # ArbitraryTile
instance (CoArbitrary a, Arbitrary a)
      => Arbitrary (Tile a) where
  arbitrary = sized $ \n ->  -- ! 1
    case n <= 1 of
      True -> pure <$> arbitrary  -- ! 2
      False -> frequency  -- ! 3
        [ (3,) $ pure <$> arbitrary  -- ! 4
        , (9,) $ beside <$> scaledAbitrary 2  -- ! 5
                        <*> scaledAbitrary 2
        , (9,) $ above <$> scaledAbitrary 2
                       <*> scaledAbitrary 2
        , (2,) $ cw <$> arbitrary
        , (2,) $ ccw <$> arbitrary
        , (4,) $ flipV <$> arbitrary
        , (4,) $ flipH <$> arbitrary
        , (6,) $ swirl <$> scaledAbitrary 4
        , (3,) $ quad <$> scaledAbitrary 4
                      <*> scaledAbitrary 4
                      <*> scaledAbitrary 4
                      <*> scaledAbitrary 4
        , (2,) $ (<*>)
              <$> scaledAbitrary @(Tile (Bool -> a)) 2
              <*> scaledAbitrary 2
        ]

scaledAbitrary :: Arbitrary a => Int -> Gen a
scaledAbitrary n = scale (`div` n) arbitrary

instance CoArbitrary PixelRGBA8 where
  coarbitrary (Color r g b a) = coarbitrary (r, g, b, a)

instance Arbitrary PixelRGBA8 where
  arbitrary = do
    a <- choose (0, 255)
    case a == 0 of
      True  -> pure mempty
      False -> PixelRGBA8 <$> choose (0,255) <*> choose (0,255) <*> choose (0,255) <*> pure a

instance Monad Tile where
  Tile ma >>= f = Tile $ \x y -> sample (f (ma x y)) x y

cw :: Tile a -> Tile a
cw (Tile f) = Tile $ \x y -> f y (negate x)

ccw :: Tile a -> Tile a
ccw (Tile f) = Tile $ \x y -> f (negate y) x

_fromImage :: Image PixelRGBA8 -> Tile Color
_fromImage img@(Image w h _) = Tile $ \x y ->
  pixelAt
    img
    (coordToPixel w x)
    (coordToPixel h y)

beside :: Tile a -> Tile a -> Tile a
beside (Tile a) (Tile b) = Tile $ \x y ->
  case x >= 0 of
    False -> a (2 * (x + 0.5)) y
    True  -> b (2 * (x - 0.5)) y

above :: Tile a -> Tile a -> Tile a
above (Tile a) (Tile b) = Tile $ \x y ->
  case y >= 0 of
    False -> a x (2 * (y + 0.5))
    True  -> b x (2 * (y - 0.5))

behind :: Tile Color -> Tile Color -> Tile Color
behind = flip (liftA2 _over)

flipH :: Tile a -> Tile a
flipH (Tile t) = Tile $ \x y ->
  t (negate x) y

flipV :: Tile a -> Tile a
flipV (Tile t) = Tile $ \x y ->
  t x (negate y)

empty :: Tile Color
empty = pure $ PixelRGBA8 0 0 0 0

rows :: Monoid a => [Tile a] -> Tile a
rows [] = pure mempty
rows ts = Tile $ \x y ->
  let h = length ts
      i = coordToPixel h y
   in sample (ts !! i) x ((y - pixelToCoord h i) * fromIntegral h)

cols :: Monoid a => [Tile a] -> Tile a
cols [] = pure mempty
cols ts = Tile $ \x y ->
  let w = length ts
      i = coordToPixel w x
   in sample (ts !! i) ((x - pixelToCoord w i) * fromIntegral w) y

quad :: Tile a -> Tile a -> Tile a -> Tile a -> Tile a
quad a b c d = (a `beside` b) `above` (c `beside` d)

quads :: (a -> a) -> Tile (a -> a)
quads f =
  quad
    (pure id)
    (pure f)
    (pure $ f . f . f)
    (pure $ f . f)

swirl :: Tile a -> Tile a
swirl t = quad t (cw t) (ccw t) $ cw $ cw t

nona :: Monoid a => Tile a -> Tile a -> Tile a -> Tile a
nona t tr c =
  rows $
       [ cols [ ccw tr ,     t,         tr    ]
       , cols [ ccw t  ,     c,         cw t  ]
       , cols [ cw (cw tr) , cw $ cw t, cw tr ]
       ]

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

_mask :: Color -> Color -> Color
_mask (PixelRGBA8 _ _ _ a) (PixelRGBA8 r g b _) = PixelRGBA8 r g b a


--------------------------------------------------------------------------------

toPNG :: Int -> Int -> Tile Color -> Image PixelRGBA8
toPNG w h t = generateImage (samplePixel w h t) w h


samplePixel
  :: Int  -- ^ width
  -> Int  -- ^ height
  -> Tile a
  -> Int  -- ^ x
  -> Int  -- ^ y
  -> a
samplePixel w h = \t x y ->
  sample t (pixelToCoord w x) (pixelToCoord h y)

coordToPixel :: Int -> Double -> Int
coordToPixel w = \x ->
  let x' = (x + 1) * fromIntegral w / 2
   in max 0 $ min (w - 1) $ floor x'

pixelToCoord :: Int -> Int -> Double
pixelToCoord w = \x ->
  let xspan = 2 / fromIntegral w
      x' = (fromIntegral x + 0.5) * xspan
   in (-1 + x')

--------------------------------------------------------------------------------


haskell :: Tile Color
haskell = unsafePerformIO $  do
  Right (ImageRGBA8 img) <- decodePng <$> BS.readFile "code/Tiles/hs.png"
  pure $ _fromImage img
{-# NOINLINE haskell #-}

sandy :: Tile Color
sandy = unsafePerformIO $  do
  Right (ImageRGBA8 img) <- decodePng <$> BS.readFile "code/Tiles/sandy2.png"
  pure $ _fromImage img
{-# NOINLINE sandy #-}

church :: Tile Color
church = unsafePerformIO $  do
  Right (ImageRGBA8 img) <- decodePng <$> BS.readFile "code/Tiles/church.png"
  pure $ _fromImage img
{-# NOINLINE church #-}

spj :: Tile Color
spj = unsafePerformIO $  do
  Right (ImageRGBA8 img) <- decodePng <$> BS.readFile "code/Tiles/spj.png"
  pure $ _fromImage img
{-# NOINLINE spj #-}



--------------------------------------------------------------------------------

__design
    :: (String, [String], [(String, String)])
    -> String
    -> String
    -> Tile Color
    -> IO ()
__design (name, _, kvs) txt hash t = do
  let fp = hash <> ".png"
      label = fromMaybe txt $ lookup "label" kvs
      figname = bool name ("fig:" <> __makeFigName label)  $ name == ""
      md = MD.singleton MD.DpiX 300 <> MD.singleton MD.DpiY 300
  Lb.writeFile fp $ encodePngWithMetadata md $ toPNG 600 600 t
  putStrLn $
    mconcat ["![", label, "](", fp, "){#", figname , "}"]

__makeFigName :: String -> String
__makeFigName
    = concatMap (\x -> bool x (take 1 x) $ take 1 x == "_")
    . group
    . fmap go
  where
    go c | isAlphaNum c = c
    go _ = '_'

main :: IO ()
main = do
  writePng "/tmp/test.png" $ toPNG 256 256 $
    let x = quad (pure const) (pure $ flip const) (pure $ flip const) (pure const)
        xx = quad x x x x
     in behind (flip beside (color 0.8 0.6 0.2 1) $ color 0 0.8 0.9 1) $ quad xx xx xx xx <*> sandy <*> flipH sandy

------------------------------------------------------------------------------
-- | Rasterize a 'Tile' down into a row-major representation of its constituent
-- "pixels".
rasterize
    :: forall a
     . Int  -- ^ resulting width
    -> Int  -- ^ resulting heigeht
    -> Tile a
    -> [[a]]  -- ^ the resulting "pixels" in row-major order
rasterize w h t = do
  y <- [0 .. (h - 1)]
  pure $ do
    x <- [0 .. (w - 1)]
    pure $ samplePixel w h t x y

_carpet :: Int -> Int -> Tile Color
_carpet 0 _ = _black
_carpet n h =
  let carpet' dh = _carpet (n - 1) (H.hash (h, dh :: Int))
   in rows
        [ cols [ carpet' 0, carpet' 1,  carpet' 2 ]
        , cols [ carpet' 3, _colors M.! (h `mod` length _colors), carpet' 4 ]
        , cols [ carpet' 5, carpet' 6,  carpet' 7 ]
        ]


_colors :: Map Int (Tile Color)
_colors = M.fromList $ zip [0..]
  [ color 1 0 0 1
  , color 1 p 0 1
  , color 1 1 0 1
  , color p 1 0 1
  , color 0 1 0 1
  , color 0 1 p 1
  , color 0 1 1 1
  , color 0 p 1 1
  , color 0 0 1 1
  , color p 0 1 1
  , color 1 0 1 1
  , color 1 0 p 1
  ]
  where
    p = 0.8

_black :: Tile Color
_black = color 0 0 0 1

