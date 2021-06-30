sig :: Sig
sig = signature
  [ sig_types
  , sig_monoid
  , sig_opts
  , sig_cons
  ]

sig_monoid :: Sig
sig_monoid = background
  [ con "mempty" $ liftC @(Monoid A) $ mempty @A
  , con "<>"     $ liftC @(Semigroup A) $ (<>)   @A
  ]

sig_opts :: Sig
sig_opts = signature
  [ variableUse Linear $  -- ! 1
      Proxy @(Challenge Test TestClue TestReward)
  , withMaxTermSize 6
  ]

sig_types :: Sig
sig_types = signature
  [ monoObserve @(Challenge Test TestClue TestReward)
  , vars ["c"] $
      Proxy @(Challenge Test TestClue TestReward)
  , monoObserve @TestReward
  , vars ["r"] $ Proxy @TestReward
  , monoObserve @(InputFilter Test)
  , vars ["f"] $ Proxy @(InputFilter Test)
  , monoVars @(Test) ["f"]
  , monoVars @(TestClue) ["k"]
  , monoVars @(Input Test) ["i"]
  , instanceOf @(Monoid TestClue)  -- ! 1
  , instanceOf @(Semigroup TestClue)  -- ! 1
  , instanceOf @(Monoid TestReward)
  , instanceOf @(Semigroup TestReward)
  ]


