module QuickSpec.Extra where

import QuickSpec

monoid :: Sig
monoid = signature
  [ con "mempty" $ liftC @(Monoid A) $ mempty @A
  , con "<>"     $ liftC @(Semigroup A) $ (<>)   @A
  , mono @[Int]
  , defaultTo $ Proxy @[Int]  -- ! 1
  , instanceOf @(Monoid [Int])
  , instanceOf @(Semigroup [Int])
  ]

