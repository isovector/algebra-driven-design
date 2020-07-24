{-# LANGUAGE FlexibleContexts #-}

module Images where

import Data.Bool
import           Data.Char
import           Diagrams.Backend.Rasterific (B)
import qualified Diagrams.Backend.Rasterific as D
import qualified Diagrams.Prelude as D
import           System.IO.Unsafe
import Data.List (group)
import Data.Maybe

type PNG = D.Diagram B

------------------------------------------------------------------------------

data Tile
  = Raw PNG
  | Empty
  | Cw Tile
  | Ccw Tile
  | Rows [Tile]
  | Cols [Tile]
  | Overlay Tile Tile
  | FlipH Tile
  | FlipV Tile

instance Semigroup Tile where
  (<>) = behind

instance Monoid Tile where
  mempty = Empty

cw :: Tile -> Tile
cw = Cw

half :: Tile -> Tile
half = Cw . Cw

ccw :: Tile -> Tile
ccw = Ccw

beside :: Tile -> Tile -> Tile
beside a b = Cols [a, b]

above :: Tile -> Tile -> Tile
above a b = Rows [a, b]

behind :: Tile -> Tile -> Tile
behind = Overlay

flipH :: Tile -> Tile
flipH = FlipH

flipV :: Tile -> Tile
flipV = FlipV

empty :: Tile
empty = Empty

rows :: [Tile] -> Tile
rows [] = Empty
rows xs = Rows xs

cols :: [Tile] -> Tile
cols [] = Empty
cols xs = Cols xs

quad :: Tile -> Tile -> Tile -> Tile -> Tile
quad a b c d = (a `beside` b) `above` (c `beside` d)

swirl :: Tile -> Tile
swirl t = quad t (cw t) (ccw t) $ cw $ cw t

nona :: Tile -> Tile -> Tile -> Tile
nona t tr c =
  rows
    [ cols [ ccw tr,     t,         tr    ]
    , cols [ ccw t,      c,         cw t  ]
    , cols [ cw $ cw tr, cw $ cw t, cw tr ]
    ]


------------------------------------------------------------------------------

toPNG :: Tile -> PNG
toPNG = _runDiagram

fromPNG :: PNG -> Tile
fromPNG = Raw . D.scaleToX 1 . D.scaleToY 1


------------------------------------------------------------------------------

haskell :: Tile
haskell = unsafePerformIO $  do
  Right png <- D.loadImageEmb @Double "code/images/hs.png"
  pure $ fromPNG $ D.image png
{-# NOINLINE haskell #-}

sandy :: Tile
sandy = unsafePerformIO $  do
  Right png <- D.loadImageEmb @Double "code/images/sandy.jpg"
  pure $ fromPNG $ D.image png
{-# NOINLINE sandy #-}


------------------------------------------------------------------------------

_scaling :: (Double -> PNG -> PNG) -> [PNG] -> [PNG]
_scaling f xs =
  let n = fromIntegral $ length xs
   in fmap (f (1 / n)) xs

_runDiagram :: Tile -> D.Diagram B
_runDiagram Empty         = D.center $ D.strutX 1 <> D.strutY 1
_runDiagram (Raw img)     = D.center img
_runDiagram (Cw x)        = D.center $ D.rotate (-1/4 D.@@ D.turn) $ _runDiagram x
_runDiagram (Ccw x)       = D.center $ D.rotate (1/4 D.@@ D.turn) $ _runDiagram x
_runDiagram (Rows xs)     = D.center $ D.vcat $ _scaling D.scaleY $ fmap _runDiagram xs
_runDiagram (Cols xs)     = D.center $ D.hcat $ _scaling D.scaleX $ fmap _runDiagram xs
_runDiagram (Overlay x y) = D.center $ _runDiagram y <> _runDiagram x
_runDiagram (FlipH x)     = D.center $ D.reflectX $ _runDiagram x
_runDiagram (FlipV x)     = D.center $ D.reflectY $ _runDiagram x

------------------------------------------------------------------------------





------------------------------------------------------------------------------

__design
    :: (String, [String], [(String, String)])
    -> String
    -> String
    -> Tile
    -> IO ()
__design (name, _, kvs) txt hash t = do
  let fp = hash <> ".png"
      label = fromMaybe txt $ lookup "label" kvs
      figname = bool name ("fig:" <> __makeFigName label)  $ name == ""
  D.renderRasterific fp (D.dims2D 96 96)  $ toPNG t
  -- TODO(sandy): bug here; the sdcp makefile should copy these files
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
  D.renderRasterific "images/test.png" (D.dims2D 512 512)  $ toPNG $
    nona (cols [haskell, behind haskell (flipH haskell), flipH haskell]) (swirl $ swirl $ swirl $ cw haskell) haskell

  pure ()

