quickspec_laws :: [(String, Property)]
quickspec_laws =
  [ ( "clue (ks <> ks2) c = clue ks (clue ks2 c)"
    , property $
        \ (ks :: [Int])
          (ks2 :: [Int])
          (c :: Challenge Test Int (MultiSet Int)) ->
            (clue (ks <> ks2) c :: Challenge Test Int (MultiSet Int)) =~= clue ks (clue ks2 c))
  , ( "empty = reward mempty"
    , property $
            (empty :: Challenge Test Int (MultiSet Int)) =~= reward mempty)
  , ( "both c c2 = both c2 c"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (both c c2 :: Challenge Test Int (MultiSet Int)) =~= both c2 c)
  , ( "eitherC c c2 = eitherC c2 c"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC c c2 :: Challenge Test Int (MultiSet Int)) =~= eitherC c2 c)
  , ( "both c c = eitherC c c"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int)) ->
            (both c c :: Challenge Test Int (MultiSet Int)) =~= eitherC c c)
  , ( "gate f bottom = bottom"
    , property $
        \ (f :: InputFilter Test) ->
            (gate f bottom :: Challenge Test Int (MultiSet Int)) =~= bottom)
  , ( "andThen c empty = c"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int)) ->
            (andThen c empty :: Challenge Test Int (MultiSet Int)) =~= c)
  , ( "andThen bottom c = bottom"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int)) ->
            (andThen bottom c :: Challenge Test Int (MultiSet Int)) =~= bottom)
  , ( "andThen empty c = c"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int)) ->
            (andThen empty c :: Challenge Test Int (MultiSet Int)) =~= c)
  , ( "both c bottom = andThen c bottom"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int)) ->
            (both c bottom :: Challenge Test Int (MultiSet Int)) =~= andThen c bottom)
  , ( "both c empty = c"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int)) ->
            (both c empty :: Challenge Test Int (MultiSet Int)) =~= c)
  , ( "eitherC c bottom = c"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int)) ->
            (eitherC c bottom :: Challenge Test Int (MultiSet Int)) =~= c)
  , ( "both c (reward r) = andThen (reward r) c"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (r :: MultiSet Int) ->
            (both c (reward r) :: Challenge Test Int (MultiSet Int)) =~= andThen (reward r) c)
  , ( "eitherC empty (reward r) = reward r"
    , property $
        \ (r :: MultiSet Int) ->
            (eitherC empty (reward r) :: Challenge Test Int (MultiSet Int)) =~= reward r)
  , ( "andThen (gate f c) c2 = gate f (andThen c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (f :: InputFilter Test) ->
            (andThen (gate f c) c2 :: Challenge Test Int (MultiSet Int)) =~= gate f (andThen c c2))
  , ( "andThen (andThen c c2) c3 = andThen c (andThen c2 c3)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (c3 :: Challenge Test Int (MultiSet Int)) ->
            (andThen (andThen c c2) c3 :: Challenge Test Int (MultiSet Int)) =~= andThen c (andThen c2 c3))
  , ( "both c (andThen c c2) = andThen (both c c) c2"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (both c (andThen c c2) :: Challenge Test Int (MultiSet Int)) =~= andThen (both c c) c2)
  , ( "both (both c c2) c3 = both c (both c2 c3)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (c3 :: Challenge Test Int (MultiSet Int)) ->
            (both (both c c2) c3 :: Challenge Test Int (MultiSet Int)) =~= both c (both c2 c3))
  , ( "eitherC c (clue k c) = both c (clue k c)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (eitherC c (clue k c) :: Challenge Test Int (MultiSet Int)) =~= both c (clue k c))
  , ( "eitherC c (both c c) = both c (both c c)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int)) ->
            (eitherC c (both c c) :: Challenge Test Int (MultiSet Int)) =~= both c (both c c))
  , ( "eitherC (eitherC c c2) c3 = eitherC c (eitherC c2 c3)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (c3 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC (eitherC c c2) c3 :: Challenge Test Int (MultiSet Int)) =~= eitherC c (eitherC c2 c3))
  , ( "andThen (reward r) (reward r2) = reward (r <> r2)"
    , property $
        \ (r :: MultiSet Int) (r2 :: MultiSet Int) ->
            (andThen (reward r) (reward r2) :: Challenge Test Int (MultiSet Int)) =~= reward (r <> r2))
  , ( "both c (clue k empty) = andThen (clue k empty) c"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (both c (clue k empty) :: Challenge Test Int (MultiSet Int)) =~= andThen (clue k empty) c)
  , ( "both c (eitherC c2 empty) = andThen (eitherC c2 empty) c"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (both c (eitherC c2 empty) :: Challenge Test Int (MultiSet Int)) =~= andThen (eitherC c2 empty) c)
  , ( "both c c = eitherC c (andThen c bottom)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int)) ->
            (both c c :: Challenge Test Int (MultiSet Int)) =~= eitherC c (andThen c bottom))
  , ( "eitherC empty (gate f c) = empty"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (f :: InputFilter Test) ->
            (eitherC empty (gate f c) :: Challenge Test Int (MultiSet Int)) =~= empty)
  , ( "eitherC empty (both c c2) = eitherC c (eitherC c2 empty)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC empty (both c c2) :: Challenge Test Int (MultiSet Int)) =~= eitherC c (eitherC c2 empty))
  , ( "andThen (reward r) (clue k empty) = clue k (reward r)"
    , property $
        \ (k :: [Int]) (r :: MultiSet Int) ->
            (andThen (reward r) (clue k empty) :: Challenge Test Int (MultiSet Int)) =~= clue k (reward r))
  , ( "andThen (reward r) (eitherC c empty) = eitherC c (reward r)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (r :: MultiSet Int) ->
            (andThen (reward r) (eitherC c empty) :: Challenge Test Int (MultiSet Int)) =~= eitherC c (reward r))
  , ( "clue k (andThen c (reward r)) = andThen (clue k c) (reward r)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int])
          (r :: MultiSet Int) ->
            (clue k (andThen c (reward r)) :: Challenge Test Int (MultiSet Int)) =~= andThen (clue k c) (reward r))
  , ( "clue k (andThen (reward r) c) = andThen (reward r) (clue k c)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int])
          (r :: MultiSet Int) ->
            (clue k (andThen (reward r) c) :: Challenge Test Int (MultiSet Int)) =~= andThen (reward r) (clue k c))
  , ( "both c (clue k (reward r)) = andThen (clue k (reward r)) c"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int])
          (r :: MultiSet Int) ->
            (both c (clue k (reward r)) :: Challenge Test Int (MultiSet Int)) =~= andThen (clue k (reward r)) c)
  , ( "eitherC c (andThen c (reward r)) = andThen (both c c) (reward r)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (r :: MultiSet Int) ->
            (eitherC c (andThen c (reward r)) :: Challenge Test Int (MultiSet Int)) =~= andThen (both c c) (reward r))
  , ( "eitherC c (andThen (reward r) c2) = andThen (reward r) (eitherC c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (r :: MultiSet Int) ->
            (eitherC c (andThen (reward r) c2) :: Challenge Test Int (MultiSet Int)) =~= andThen (reward r) (eitherC c c2))
  , ( "both (clue k c) (clue k c) = clue k (both c c)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (both (clue k c) (clue k c) :: Challenge Test Int (MultiSet Int)) =~= clue k (both c c))
  , ( "both (gate f c) (gate f c2) = gate f (both c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (f :: InputFilter Test) ->
            (both (gate f c) (gate f c2) :: Challenge Test Int (MultiSet Int)) =~= gate f (both c c2))
  , ( "both (andThen c c2) (andThen c c3) = andThen (both c c) (both c2 c3)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (c3 :: Challenge Test Int (MultiSet Int)) ->
            (both (andThen c c2) (andThen c c3) :: Challenge Test Int (MultiSet Int)) =~= andThen (both c c) (both c2 c3))
  , ( "eitherC (clue k c) (clue k c2) = clue k (eitherC c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (eitherC (clue k c) (clue k c2) :: Challenge Test Int (MultiSet Int)) =~= clue k (eitherC c c2))
  , ( "eitherC (clue k c) (clue k2 c) = both (clue k c) (clue k2 c)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int])
          (k2 :: [Int]) ->
            (eitherC (clue k c) (clue k2 c) :: Challenge Test Int (MultiSet Int)) =~= both (clue k c) (clue k2 c))
  , ( "eitherC (clue k c) (both c c) = both (clue k c) (both c c)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (eitherC (clue k c) (both c c) :: Challenge Test Int (MultiSet Int)) =~= both (clue k c) (both c c))
  , ( "eitherC (gate f c) (gate f c2) = gate f (eitherC c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (f :: InputFilter Test) ->
            (eitherC (gate f c) (gate f c2) :: Challenge Test Int (MultiSet Int)) =~= gate f (eitherC c c2))
  , ( "eitherC (andThen c c2) (andThen c c3) = andThen (both c c) (eitherC c2 c3)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (c3 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC (andThen c c2) (andThen c c3) :: Challenge Test Int (MultiSet Int)) =~= andThen (both c c) (eitherC c2 c3))
  , ( "eitherC (both c c2) (eitherC c c2) = both (eitherC c c2) (eitherC c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC (both c c2) (eitherC c c2) :: Challenge Test Int (MultiSet Int)) =~= both (eitherC c c2) (eitherC c c2))
  , ( "andThen (clue k c) (clue k bottom) = andThen (clue k c) bottom"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (andThen (clue k c) (clue k bottom) :: Challenge Test Int (MultiSet Int)) =~= andThen (clue k c) bottom)
  , ( "andThen (clue k c) (clue k empty) = clue k c"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (andThen (clue k c) (clue k empty) :: Challenge Test Int (MultiSet Int)) =~= clue k c)
  , ( "both (clue k c) (clue k bottom) = andThen (clue k c) bottom"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (both (clue k c) (clue k bottom) :: Challenge Test Int (MultiSet Int)) =~= andThen (clue k c) bottom)
  , ( "eitherC (clue k c) (andThen c bottom) = both c (clue k c)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (eitherC (clue k c) (andThen c bottom) :: Challenge Test Int (MultiSet Int)) =~= both c (clue k c))
  , ( "eitherC (gate f c) (andThen c bottom) = both c (gate f c)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (f :: InputFilter Test) ->
            (eitherC (gate f c) (andThen c bottom) :: Challenge Test Int (MultiSet Int)) =~= both c (gate f c))
  , ( "eitherC (andThen c c2) (andThen c2 bottom) = both c2 (andThen c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC (andThen c c2) (andThen c2 bottom) :: Challenge Test Int (MultiSet Int)) =~= both c2 (andThen c c2))
  , ( "eitherC (andThen c bottom) (both c c2) = both c (both c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC (andThen c bottom) (both c c2) :: Challenge Test Int (MultiSet Int)) =~= both c (both c c2))
  , ( "eitherC (andThen c bottom) (both c2 c2) = eitherC c2 (both c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC (andThen c bottom) (both c2 c2) :: Challenge Test Int (MultiSet Int)) =~= eitherC c2 (both c c2))
  , ( "andThen (clue k empty) (eitherC c empty) = eitherC c (clue k empty)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (andThen (clue k empty) (eitherC c empty) :: Challenge Test Int (MultiSet Int)) =~= eitherC c (clue k empty))
  , ( "andThen (eitherC c empty) (eitherC c2 empty) = eitherC c (eitherC c2 empty)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (andThen (eitherC c empty) (eitherC c2 empty) :: Challenge Test Int (MultiSet Int)) =~= eitherC c (eitherC c2 empty))
  , ( "eitherC (andThen c bottom) (andThen c2 bottom) = andThen (both c c2) bottom"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC (andThen c bottom) (andThen c2 bottom) :: Challenge Test Int (MultiSet Int)) =~= andThen (both c c2) bottom)
  , ( "andThen c (both c (clue k c)) = andThen c (clue k (both c c))"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (andThen c (both c (clue k c)) :: Challenge Test Int (MultiSet Int)) =~= andThen c (clue k (both c c)))
  , ( "andThen (both c (clue k c)) c2 = both (clue k c) (andThen c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (andThen (both c (clue k c)) c2 :: Challenge Test Int (MultiSet Int)) =~= both (clue k c) (andThen c c2))
  , ( "andThen (both c (both c c)) c2 = both (andThen c c2) (both c c)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (andThen (both c (both c c)) c2 :: Challenge Test Int (MultiSet Int)) =~= both (andThen c c2) (both c c))
  , ( "andThen (both c (eitherC c c2)) c3 = both (andThen c c3) (eitherC c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (c3 :: Challenge Test Int (MultiSet Int)) ->
            (andThen (both c (eitherC c c2)) c3 :: Challenge Test Int (MultiSet Int)) =~= both (andThen c c3) (eitherC c c2))
  , ( "both c (clue k (both c c)) = both (clue k c) (both c c)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (both c (clue k (both c c)) :: Challenge Test Int (MultiSet Int)) =~= both (clue k c) (both c c))
  , ( "both c (gate f (andThen c c2)) = andThen (both c (gate f c)) c2"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (f :: InputFilter Test) ->
            (both c (gate f (andThen c c2)) :: Challenge Test Int (MultiSet Int)) =~= andThen (both c (gate f c)) c2)
  , ( "both c (andThen c2 (andThen c c3)) = andThen (both c (andThen c2 c)) c3"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (c3 :: Challenge Test Int (MultiSet Int)) ->
            (both c (andThen c2 (andThen c c3)) :: Challenge Test Int (MultiSet Int)) =~= andThen (both c (andThen c2 c)) c3)
  , ( "both c (andThen (clue k c) c2) = both (clue k c) (andThen c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (both c (andThen (clue k c) c2) :: Challenge Test Int (MultiSet Int)) =~= both (clue k c) (andThen c c2))
  , ( "both c (andThen (both c c2) c3) = andThen (both c (both c c2)) c3"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (c3 :: Challenge Test Int (MultiSet Int)) ->
            (both c (andThen (both c c2) c3) :: Challenge Test Int (MultiSet Int)) =~= andThen (both c (both c c2)) c3)
  , ( "both c (eitherC c (gate f c)) = eitherC (gate f c) (both c c)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (f :: InputFilter Test) ->
            (both c (eitherC c (gate f c)) :: Challenge Test Int (MultiSet Int)) =~= eitherC (gate f c) (both c c))
  , ( "both c (eitherC c (andThen c c2)) = eitherC (andThen c c2) (both c c)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (both c (eitherC c (andThen c c2)) :: Challenge Test Int (MultiSet Int)) =~= eitherC (andThen c c2) (both c c))
  , ( "both c (eitherC c (andThen c2 c)) = eitherC (andThen c2 c) (both c c)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (both c (eitherC c (andThen c2 c)) :: Challenge Test Int (MultiSet Int)) =~= eitherC (andThen c2 c) (both c c))
  , ( "both c (eitherC c (both c c2)) = eitherC (both c c) (both c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (both c (eitherC c (both c c2)) :: Challenge Test Int (MultiSet Int)) =~= eitherC (both c c) (both c c2))
  , ( "eitherC c (clue k (clue k2 c)) = both c (clue k (clue k2 c))"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int])
          (k2 :: [Int]) ->
            (eitherC c (clue k (clue k2 c)) :: Challenge Test Int (MultiSet Int)) =~= both c (clue k (clue k2 c)))
  , ( "eitherC c (clue k (both c c)) = both (clue k c) (both c c)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (eitherC c (clue k (both c c)) :: Challenge Test Int (MultiSet Int)) =~= both (clue k c) (both c c))
  , ( "eitherC c (andThen (clue k c) c2) = eitherC (clue k c) (andThen c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (eitherC c (andThen (clue k c) c2) :: Challenge Test Int (MultiSet Int)) =~= eitherC (clue k c) (andThen c c2))
  , ( "eitherC c (both c2 (clue k c)) = eitherC (clue k c) (both c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (eitherC c (both c2 (clue k c)) :: Challenge Test Int (MultiSet Int)) =~= eitherC (clue k c) (both c c2))
  , ( "eitherC c (both c (eitherC c c2)) = both (both c c) (eitherC c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC c (both c (eitherC c c2)) :: Challenge Test Int (MultiSet Int)) =~= both (both c c) (eitherC c c2))
  , ( "eitherC c (both c2 (eitherC c c2)) = both (eitherC c c2) (eitherC c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC c (both c2 (eitherC c c2)) :: Challenge Test Int (MultiSet Int)) =~= both (eitherC c c2) (eitherC c c2))
  , ( "andThen (clue k (andThen c bottom)) c2 = clue k (andThen c bottom)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (andThen (clue k (andThen c bottom)) c2 :: Challenge Test Int (MultiSet Int)) =~= clue k (andThen c bottom))
  , ( "both c (eitherC c (andThen c2 bottom)) = eitherC c (both c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (both c (eitherC c (andThen c2 bottom)) :: Challenge Test Int (MultiSet Int)) =~= eitherC c (both c c2))
  , ( "eitherC c (andThen c2 (andThen c bottom)) = eitherC c (andThen c2 c)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC c (andThen c2 (andThen c bottom)) :: Challenge Test Int (MultiSet Int)) =~= eitherC c (andThen c2 c))
  , ( "eitherC c (andThen (clue k empty) c2) = andThen (clue k empty) (eitherC c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (k :: [Int]) ->
            (eitherC c (andThen (clue k empty) c2) :: Challenge Test Int (MultiSet Int)) =~= andThen (clue k empty) (eitherC c c2))
  , ( "eitherC c (andThen (eitherC c c2) bottom) = both c (eitherC c c2)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC c (andThen (eitherC c c2) bottom) :: Challenge Test Int (MultiSet Int)) =~= both c (eitherC c c2))
  , ( "eitherC c (andThen (eitherC c2 empty) c3) = andThen (eitherC c2 empty) (eitherC c c3)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (c3 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC c (andThen (eitherC c2 empty) c3) :: Challenge Test Int (MultiSet Int)) =~= andThen (eitherC c2 empty) (eitherC c c3))
  , ( "eitherC empty (clue k (gate f c)) = eitherC empty (clue k bottom)"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (f :: InputFilter Test)
          (k :: [Int]) ->
            (eitherC empty (clue k (gate f c)) :: Challenge Test Int (MultiSet Int)) =~= eitherC empty (clue k bottom))
  , ( "eitherC empty (andThen c (gate f c2)) = eitherC c empty"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (f :: InputFilter Test) ->
            (eitherC empty (andThen c (gate f c2)) :: Challenge Test Int (MultiSet Int)) =~= eitherC c empty)
  , ( "eitherC empty (andThen c (both c c2)) = eitherC empty (andThen c (andThen c c2))"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC empty (andThen c (both c c2)) :: Challenge Test Int (MultiSet Int)) =~= eitherC empty (andThen c (andThen c c2)))
  , ( "eitherC empty (andThen c (eitherC c2 c3)) = eitherC empty (andThen c (both c2 c3))"
    , property $
        \ (c :: Challenge Test Int (MultiSet Int))
          (c2 :: Challenge Test Int (MultiSet Int))
          (c3 :: Challenge Test Int (MultiSet Int)) ->
            (eitherC empty (andThen c (eitherC c2 c3)) :: Challenge Test Int (MultiSet Int)) =~= eitherC empty (andThen c (both c2 c3)))
  ]
