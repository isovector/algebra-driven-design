## Property-Based Testing {#sec:quickcheck}

During my previous career as a software engineer, I once took part in a coding
workshop, designed to help you think more clearly about programming. The
exercises were structured around Conway's Game of Life; every thirty minutes
we'd get a new partner and a new set of constraints for how to write Game of
Life. The conditions were wacky; for example, one was that you had to run your
tests every two minutes, and if they didn't pass (or compile,) you had to
discard the last two minutes of work you'd done. But the exercise that has
always stuck with me was an adversarial one. One partner would write tests for
Game of Life and the other partner would, behind closed doors, write code that
made those tests turn green. The tester wasn't allowed to see the
implementation but was encouraged to add more tests every time the implementer
got the tests to pass. This led to an arms race, with the tester trying to best
specify the problem, and the implementer trying to do the least work possible.

I was the implementer, and my partner punched me afterward when he saw
the code I'd written. I'd never seen anyone so furious before in my life. Why?
Because, while my partner thought he was adequately specifying what a Game of
Life implementation should look like, and I was aggressively cutting corners. When
his test said that a specific board configuration should evolve into another, I
simply hard-coded that input/output pair into my implementation. By the end of
the time limit, all of his tests were green, but my implementation was
equivalent to this:

```haskell
gameOfLife "X" = " "
gameOfLife "\nXXX\n" = " X \n X \n X "
gameOfLife "XX\nXX" = "XX\nXX"
```

and so on. I hadn't done any work *whatsoever* --- I was just copying his tests
and giving back the expected output. Giving me more credit than my
duplicitous self deserved, my partner thought that I was acting in good faith.
He felt that I had an efficient implementation and that he was merely
helping think of tricky test cases. To this day I'm still not sure if this was
the intended takeaway from the exercise, but what I learned is never to trust
unit tests, and he learned not to trust coworkers any further than you can
throw them.

Imagine instead that I had been a malicious actor. By knowing what the tests
looked like, I could accurately determine whether or not my code was being
run in "test mode," and could thus branch on that information --- despite this
not being any *explicit parameter* of my function. The code I actually wrote would crash if it
encountered an unexpected input, but an unsavory actor could have instead launched nuclear
missiles. Admittedly, this is an extreme example, but it serves to illustrate
the failure of unit tests to specify programs accurately. This is the most
damning argument against test-driven development I could ever imagine. While
it's unlikely that your coworkers will be actively fooling your tests, how can
we be sure that our tests aren't actively misleading us? That is to say, how can we
convince ourselves from a finite number of finitely-sized tests that our program
is working as intended?

There is a separate but related argument against unit testing, namely that it
can't possibly scale. To echo @hughes_experiences_2016 again, unit tests require
$O(n^k)$ amounts of work to find integration bugs between $k$ different
features.

One solution is to write the first-order (single feature) unit tests, and then
simply throw up our hands saying, "we've done our best." This may or may not be
accurate, but certainly doesn't help us write better software. The problem here is
not that testing is *hard;* it's that it's *tedious,* and this boredom quickly
overwhelms our sense of engineering integrity. When phrased like this, the
solution is obvious. Like any other tedious thing, we should just automate the
process of writing tests.

The question we're left with is, "how do we programmers come up with unit tests?"
If we could only formulate and reify that knowledge, we could ask the computer
to do it for us. So, how *do* programmers come up with unit tests? They start by
having a mental model of their software, how it works, and what sorts
of invariants they can expect it to maintain. They then arbitrarily generate a
(usually) simple example of input, run it through their system, and check that
one or more of its post-conditions hold. If they are good programmers, they will
also add some tests that see what happens when the program is given unrealistic
or garbage input, though this seems to be quite rare in the real world. For
whatever reason, humans appear to be very biased towards testing and reasoning
about the "happy path" through their program. These happy paths are usually very
narrow.

With this in mind, we can decompose the task of writing unit tests to the
following parts:

1. Generate random inputs
2. Ensure our post-conditions hold

Being able to generate well-formed pieces of random data sounds at first blush
like it would not be incredibly helpful. But this is not so. One of the
primary uses of random data in computer science is using the Monte Carlo
method --- a technique for probabilistically solving problems. Of particular
interest to us is in testing our observations against random data. If a
proposition about an algebra holds after 10,000 randomly sampled points in
its input space, we can be reasonably confident it will continue to hold
for the 10,001st. Of course, we can never be *sure* about this, but we can
become arbitrarily convinced by testing larger and larger sample sizes.

In this chapter, we will look at how to go about generating useful random data.

The equations we've derived throughout this book (and the ones that we will
automatically discover when working through @sec:quickspec) will serve as our
post-condition checks.


### Basics

Random data for type `T` is produced by a *`T`-generator* whose type is `Gen T`.
The `Gen` monad can produce random numbers and has an associated
size for controlling roughly how large the resulting data should be. The size
parameter is useful when dealing with infinite large nested structures --- by
decreasing the size every time we recursively create a substructure, we can
force eventual termination. Size can also be used to explore different parts of
a data structure; when generating a random `Int`, smaller sizes will look at
values closer to 0 on average. The size parameter corresponds
roughly with the human notion of "complexity," but there is no official metric. Still, a generator should, on average, produce simpler pieces of data given a
smaller size.

Although the `Gen` monad gives us access to random numbers, it is more
convenient to use its algebraic constructors. The combinators described in this
section come from the `QuickCheck` library and are of particular interest when
writing generators. The first combinator is:

```haskell
arbitrary :: Arbitrary a => Gen a
```

which is a generator for type `a`, so long as it has an instance of `Arbitrary`.
Different libraries vary on whether or not they support `arbitrary`, but we will use it for all of our generators in this book. Another interesting generator is

```haskell
pure :: a -> Gen a
```

This function creates a constant generator that always produces its argument.
In fact, `Gen` is a full-blown monad, so it also comes equipped with `join`,
`(<*>)` and `fmap`, though they're not used as commonly.

In order to inject some randomness into our selection, we can instead use:

```haskell
oneof :: [Gen a] -> Gen a
```

which chooses one of the generators provided at random, and then produces a
value with that generator. To illustrate, we can combine `oneof` and `pure` to
create a generator for booleans:

```haskell
bool :: Gen Bool
bool = oneof
  [ pure True
  , pure False
  ]
```

While `oneof` works well for simple data, many types contain *particularly
interesting* values which you might want to produce more often, or *particularly
big* values, which you might want to produce less often. For cases like these,
we can instead use `frequency`:

```haskell
frequency :: [(Int, Gen a)] -> Gen a
```

which chooses one generator in the list at random, as weighted by their
associated integers. For example, `frequency [(3, a), (1, b)]` will generate a
value using `a` 75% of the time, and using `b` in 25% of cases.

Another useful function is `vectorOf`, which allows us to sample a generator
several times:

```haskell
vectorOf :: Int -> Gen a -> Gen [a]
```

The argument to `vectorOf` describes the size of the resulting generated list.
Of course, each element in the list will have been generated randomly and
independently of every other value. Thus we could model a series of 20 coin
flips via the generator `vectorOf 20 bool`. If you just want a random
list of any size, consider using `listOf :: Gen a -> Gen [a]` instead.

Sometimes you have a generator that you'd like to refine subtly and ensure the
values it produces satisfy a post-condition. For cases like those, we turn to
`suchThat`:

```haskell
suchThat :: Gen a -> (a -> Bool) -> Gen a
```

This combinator promises that the `a` it generates will satisfy the predicate.
But you'll notice that this is not a solvable problem in general, and isn't
guaranteed to terminate given a non-trivial predicate. If your post-condition is
tricky, consider creating a dedicated generator that only produces correct values
without needing to prune them.  For example, `suchThat myGen (>= 5)` is OK, but
`suchThat myGen (\x -> isPrime x && x >= 100000)` might take a while to find
examples.

The last two combinators of particular interest to us are `sized` and `resize`,
which respectively give you access to and change the test size.

```haskell
sized :: (Int -> Gen a) -> Gen a
resize :: Int -> Gen a -> Gen a
```

In QuickCheck, an implicit parameter is passed to every generator called
the size parameter, which describes how "big" produced values should be. There
is no prescribed meaning to the size parameter, other than generators should
make more complicated outputs when the size parameter is bigger. However, the exact
semantics for this are left unspecified. For an integer, the size might describe
the magnitude of the largest possible integer produced or the frequency
at which very large integers are generated. For lists, you might get longer lists
with larger elements when the size parameter is big.

The `sized` combinator passes the current value of the size parameter to your
function, allowing you to use it to generate appropriately sized values. I
typically use this to prune recursive types to a sufficient depth. The
`resize` parameter can be used to change the size parameter when invoking a
different generator --- useful for making sure the sub-expressions you generate
are progressively smaller.

The above constructors are deceptively simple. We can use them to generate
relatively complicated things, like binary trees:

```haskell
leaf   :: a -> BTree a
branch :: BTree a -> BTree a -> BTree a

btreeGen :: Arbitrary a => Gen (BTree a)
btreeGen = sized $ \size ->  -- ! 1
  frequency
    [ (1, fmap leaf arbitrary)  -- ! 2
    , (size, do  -- ! 3
        lside <- resize (div size 2) btreeGen  -- ! 4
        rside <- resize (div size 2) btreeGen
        pure (branch lside rside)  -- ! 5
    ]
```

The `btreeGen` generator has many fascinating things going on, as indicated
above. At `Ann:1`, we get hold of the size parameter and bind it to a variable
named `size`. At `Ann:2`, one of our options is the leaf node with an
`arbitrary` value inside of it. The frequency of this is 1, meaning it is always
a contender. Because the frequency at `Ann:3` is `size`, our `leaf` case becomes
more likely the smaller `size` is. At `Ann:4` we generate a smaller `BTree a`
using `btreeGen` itself, but after halving its size parameter. Finally at
`Ann:5` we use `pure` to return a "constant" `branch` constructor, although one
with random subtrees.

This pattern of getting the size and using it to bias generators away from
trivial constructors is common. You'll notice that at `Ann:4`, we cut our
size parameter in half, rather than merely decrementing it. As a result, the
largest `BTree`s that can be generated by `btreeGen` have $O(size)$ leaves and,
correspondingly, a height bounded by $O(\log_2{(size)})$. This choice is driven
pragmatically, as recall that our primary purpose in generating random data is
for Monte Carlo simulations --- that is, we will be producing and
testing a great deal of them. It is prudent that the terms created by a
generator not grow super-linearly on average with respect to the size
parameter; unnecessarily slow-to-check terms don't do anyone any favors.


### Writing Good Generators

Unfortunately, because generators are code, the responsibility for writing
*good generators* falls on our shoulders. This is one area in which the computer
is unable to help us, and as such, requires extreme vigilance. A bad
generator's cost is untruthful test results, and worse, these failings are usually
silent. The good news is that there are no insidious bugs that arise when
writing generators, following the checklist below is sufficient to avoid
problems 99% of the time.

The most important thing you can do to write a good generator is to write a test
that shows your generator produces only valid input. We can do this by writing a
function that validates its input and returns `True` if all invariants are met.
Perhaps we have a data structure that corresponds to actions we can do with a
shared counter that requires a lock to manipulate.

```haskell
data Counter
  = WithLock Counter
  | Increment Int
  | Decrement Int
```

Our invariants might be that `WithLock` may not occur inside of itself --- doing
so would lead to a deadlock --- and that the integers on `Increment` and
`Decrement` must be greater than zero. We can encode these invariants as a
predicate:

```haskell
validCounter :: Counter -> Bool
validCounter (WithLock (WithLock _)) = False
validCounter (WithLock c)  = validCounter c
validCounter (Increment n) = n > 0
validCounter (Decrement n) = n > 0
```

We can then test our generator by writing the following QuickCheck test:

```haskell
prop_validGenerator :: Property
prop_validGenerator = property validCounter
```

If this test ever fails, it means our generator is producing invalid values.
Such a thing acts as a smoke-test and first line of defense against crazy bugs.
If you've written a broken generator, there is no telling what sorts of insane behavior
you'll see throughout the remainder of the process. **Don't skip this step!**

Rather than diving further into the checklist of how to write good
generators, let's instead take some time and look at what goes *wrong* with a
bad generator. With luck, being familiar with these issues will save you many
hours of debugging a bad generator in the off-chance it happens.


#### Anti-pattern: Not Generating All Constructors {.unlisted .unnumbered}

The most obvious problem when writing generators is forgetting to generate some
of your algebra's constructors. The issue here is that entire swathes of
possible terms in your algebra will go untested, and many of the promises of
Algebra-Driven Design are broken. Because generators are used to stamp out unit
tests, bugs pertaining to the forgotten constructors can't be flagged.
Furthermore, when working with an automatic specification of laws (@sec:quickspec),
we will find contradictory laws --- and often, this is our only means of
noticing generators that are missing constructors.

As an example of this, when preparing material for another chapter, QuickSpec
discovered the following pair of laws:

```haskell
 1. both s s2 = both s2 s
 2. both (set c y) (set c x) = set c x
```

Law 1 clearly shows that `both` is commutative, that the order of its arguments
doesn't matter in the least. Law 2, on the other hand, shows that `both` is
right-biased when it comes to the `set` constructor. These two laws can't
simultaneously be true![^set-is-wrong] The problem here was that my generator
forgot to produce `set` terms, so the right-bias for `set` was never exercised
when testing the first law. Contradictory laws like these are a good sign of a
forgotten constructor, and QuickSpec is helpful to point out which one.

[^set-is-wrong]: One possible resolution of this seeming contradiction is that
  `set` ignores its second argument. Manual testing of `set` showed this was not
  true.

This anti-pattern strikes more dangerously when dealing with evolving codebases.
It's very reasonable that the original author ensured every constructor was
accounted for by the generator, but subsequent maintenance programmers might not
even know about the generator. A new constructor might be added,
without changing the corresponding generator. The result is two coupled, but now unsynchronized, pieces of code.

**Mitigating this anti-pattern:** Discipline and process are, unfortunately, the
best mitigation. Make it a part of the code review process to double-check
that your generators agree with the constructors. Add comments to any data-types
that have associated generators, reminding people to update the generator
whenever monkeying with the data definitions.

However, that doesn't mean we are stuck. Almost by definition, changing true
constructors requires changing the underlying implementation, which we can tap
into to help generate warnings. In our `BTree` example above, we have two
constructors:

```haskell
leaf   :: a -> BTree a
branch :: BTree a -> BTree a -> BTree a
```

which likely act as the algebraic interface to a data-type similar to

```haskell
data BTree a
  = Leaf a
  | Branch (BTree a) (BTree a)
```

When compiling with the `-Wall` flag, Haskell will warn us about incomplete
pattern matches, and we can use this to our advantage. By leaving around an
unused function whose only purpose is to pattern match on the implementation of
a `BTree`, we can ask Haskell to issue warnings to us whenever the underlying
implementation is changed. This might look something like an unexported
function:

```haskell
-- | This function seems silly, but exists to ensure that
-- if you change the data constructors of 'BTree' that
-- you remember to update its Arbitrary instance.
--
-- **Failure to do so will bring financial ruin to us and
-- everyone we've ever loved.**
dontForgetToUpdateTheGenerator :: BTree a -> ()
dontForgetToUpdateTheGenerator Leaf{}   = ()
dontForgetToUpdateTheGenerator Branch{} = ()
```

The ominous comment is mandatory.

Now, whenever someone fiddles with the definition of `BTree` itself, if they add
or remove a new piece of implementation, the Haskell compiler will helpfully
give a warning pointing at this function, whose name and documentation can
remind maintenance programmers about this necessary dependency.


#### Anti-pattern: Generating Implementations Directly {.unlisted .unnumbered}

While an algebra's constructors often correlated closely with the
implementation's data representation, these are by no means the same thing. As
far as your users (and therefore, tests) are concerned, there is no
distinction between the algebra's public interface and its inner workings. To maintain this suspension of disbelief, your generators must generate
terms through the public interface, and never via the internal data
representation.

In Haskell, this means we are disallowed from ever generating the data
constructors of our algebra directly. Returning again to our binary tree
example, the following is a **terrible arbitrary instance and should never ever
be written:**

```haskell
instance Arbitrary a => Arbitrary (BTree a) where
  arbitrary = oneof
    [ Leaf   <$> arbitrary
    , Branch <$> arbitrary <*> arbitrary
    ]
```

Why? We're generating terms directly via the data constructors (`Leaf` and
`Branch`) of our type. But this is atypical of how users of our library will
generate terms, as we won't export those constructors! Instead, we will export
our "smart constructors" `leaf` and `branch`, which may or may not align with
their data constructors. By separating them, we introduce an abstraction barrier
between the *interface* of our types and their *implementation.* Our generator should instead look like this:

```haskell
instance Arbitrary a => Arbitrary (BTree a) where
  arbitrary = oneof
    [ leaf   <$> arbitrary
    , branch <$> arbitrary <*> arbitrary
    ]
```

The change is subtle, but it now uses the public interface of `BTree`. In making
this edit, the generator is now capable of creating *exactly the same set of
terms* that a library user can. This is just not the case when working directly with the
underlying representation! As such, this instance tests user-facing
behavior, while the earlier version tests nothing of interest.

To hammer this point home, let's consider the typical pattern in which a
constructor's implementation performs term simplification and maintains an
invariant before returning its result. To illustrate, @hughes_design_1995 gives
the example of a union representation, with the invariant

> that the first line of every layout in the left operand of union must be
> strictly longer than the first line of every layout in the right operand.

For Hughes, unions are a feature of the implementation landscape, not of the
algebra itself. Some constructors might be implemented in terms of this union,
but they are "smart" and responsible for maintaining the invariant. Using the
constructors in our generator, rather than generating the data representation
directly, we don't need to worry about generating data that satisfies the invariant.
Tests failing against the public interface are
more suitable to write and always more conclusive of bugs.

**Mitigating this anti-pattern:** Lift generators over your public algebraic
constructors, and never over the underlying data representation.


#### Anti-pattern: Generating Asymptotically Adversarial Inputs {.unlisted .unnumbered}

Random generation of inputs is useful for testing diverse code-paths, but what can
we do when some of the inputs hit uncharacteristic, yet asymptotically bad
cases? Here is an area where property testing grinds to a halt. If we're running
tens of thousands of tests and each takes a non-negligible amount of time to
complete, your test suite is going to become unbearably slow.

If there is an easy fix for the asymptotics --- for example,
something being accidentally quadratic --- just make the change. For more
systematic issues, we can try some band-aids as a best-effort attempt at
improving the situation, rather than putting a good deal of work into our
testing infrastructure. The easiest drop-in solution is simply to timeout
any slow-running computations. These tests are discarded, causing QuickCheck to
generate a new input and try again, hopefully, not hitting the
adversarial case. This pattern is encapsulated with the following
combinator:

```haskell
import Control.Exception
import System.Timeout

quickly :: (Eq a, Show a) => a -> a -> Property
quickly a b =
  ioProperty
    $ fmap (fromMaybe discard)
    $ timeout 1
    $ evaluate
    $ a `seq`
      b `seq`
      a === b
```

If your test is hanging on the rare asymptotically-bad case, try dropping
in a call to `quickly`. It might solve your issues without any further work. And
hopefully, further down the line, you'll find some algebraic means of improving
the performance so that this workaround is no longer necessary.

**Mitigating this anti-pattern:** Try dropping in `quickly` wrappers on
expensive test observations.


#### Anti-pattern: Generating Extremely Large Inputs {.unlisted .unnumbered}

In circumstances in which `quickly` isn't a quick fix, we can instead attempt to
tune our generators. QuickCheck uses the aforementioned size parameter to signal
to generators how big their output should be. We can hook into this machinery
and use it to inform our choices. Unfortunately, out of the box, it's not as
simple as it should be, and so we will introduce a few helpers. The function
`subtermFrequency` below is an extension to QuickCheck's `frequency` combinator,
but allows us to annotate sub-generators with an associated complexity.

```haskell
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
```

The `Freq` parameter associated with each `Gen a` is either `Cheap` or
`Expensive n`, for some integer `n` that describes roughly how expensive the
generator's output is to observe. When combined with `smallerGen` below,
generators will choose expensive terms exponentially less often the deeper they
recurse. This doesn't eliminate all adversarial cases, but it can drastically reduce their likelihood with a little hand-tuning. Compared to merely using
`quickly` as above, `subtermFrequency` often improves test-suite durations by an
order of magnitude or two.

```haskell
smallerGen :: Int -> Gen a -> Gen a
smallerGen n = scale (`div` n)

smallerArbitrary :: Arbitrary a => Int -> Gen a
smallerArbitrary n = smallerGen n arbitrary
```

As an example, the following is a real `Arbitrary` instance I wrote for a
regular expression matcher. Unfortunately, the Kleene star operator shows
exponential amounts of backtracking when nested with itself. While some
algebraic properties (such as `star (star x) = star x`) help, it's impossible to
collapse all nested `star` constructors. Instead, I've tuned the generator to
exponentially avoid nested calls to `star`.

```haskell
instance (Eq a, Arbitrary a) => Arbitrary (Regex a) where
  arbitrary = subtermFrequency
    [ (Cheap, char <$> arbitrary)
    , (Cheap, pure emptyStr)
    , (Cheap, pure failMatch)
    , (Expensive 4, star <$> smallerArbitrary 2)
    , (Expensive 2, cat <$> smallerArbitrary 2
                        <*> smallerArbitrary 2)
    , (Expensive 2, choose <$> smallerArbitrary 2
                           <*> smallerArbitrary 2)
    , (Expensive 3, optional <$> smallerArbitrary 2)
    ]
```

There are two different parameters to tune here. The argument to `Expensive` is
the inverse frequency that the generator will choose this branch, given by $F =
s/2n$, where `s` is the current size. The argument to `smallerArbitrary`, on the
other hand, describes how much simpler this constructor's subterms should be. By
making the `Expensive` argument large, you will discourage the generator from
producing this constructor *entirely*, while increasing the `smallerArbitrary`
argument, you will limit the generator from *nesting* expensive
constructors. Getting it just right requires some amount of trial and error.

Finally, if neither approach works, it's also acceptable (though frowned upon)
to reduce QuickCheck's test size across the board. Doing so will decrease your tests' quality, but imperfect tests are better than tests that can't be
run due to never terminating. The test size can be adjusted by changing your
top-level call from `quickCheck` to `quickCheckWith (stdArgs { maxSize = 10 })`.
The default size is 20, and can be adjusted to taste. This is sufficient to get
a basic test suite up and running but isn't advised for any long-living
codebases; instead, put that effort into better tuning your generators to avoid
the adversarial cases.

**Mitigating this anti-pattern:** Hand-tune your generators with
`subtermFrequency` and `smallerGen` to preferentially avoid massively
recursive cases.


#### Anti-pattern: Generating Non-Representative Terms {.unlisted .unnumbered}

One of the most worrisome and well-founded concerns with automated testing is
the anxiety that data of a specific shape will never be generated, and thus never
test problematic cases. This is a generalized version of the *Not Generating All
Constructors* anti-pattern, though less troublesome and somewhat easier to deal
with.

When debugging generators, the common problems to check are, "does this generate
fail to produce any particular values" and, "is it well-tuned in its
distribution?" As a general rule, algebraic tests should only ever test
observations, as nothing else in the algebra has any specified meaning. However,
when testing the generators themselves, it's OK to relax this restriction. Here
we can use the `Data` typeclass to lookup type information at runtime. The
function `show . toConstr` will give us a string representation of the name of
the underlying data constructor produced by a generator. Unfortunately, we can't
get our hands on the algebraic constructor used, but this is better than
nothing.

By using QuickCheck's `collect` function, we can attach a label to a property,
which will record information about the results of our generator. The final
product looks like this:

```haskell
generatorResults :: (Show a, Data a) => Gen a -> Property
generatorResults
  = ioProperty
  . labelledExamplesResult
  . forAllShow gen (const "")
  $ \p -> classify True (show $ toConstr p) True


-- | Ensures a generator produces every possible data
-- constructor.
producesAllValues
    :: forall a
     . Data a
    => Gen a
    -> Property
producesAllValues gen = ioProperty $ do
  res <- labelledExamplesResult
       $ forAllShow gen (const "") $ \p ->
            classify True (show $ toConstr p) $ True
  let all_cons = S.fromList
                 $ fmap show
                 $ dataTypeConstrs
                 $ dataTypeOf @a undefined
      gen_cons = M.keysSet $ classes res
      missing_cons = all_cons S.\\ gen_cons

  pure $ flip whenFail (null missing_cons) $ do
    putStrLn "Generator failed to produce the following:
    for_ missing_cons $ putStrLn . mappend "• "
```

The property `generatorResults` ensures that a generator produces every possible
data constructor, and as a bonus, will give examples of terms it generated. It
will also list the frequency at which it generates these data constructors.

Unfortunately, these combinators don't exist in any library I know of; my
workflow involves just schlepping them around as I need them. It's awkward, but
perhaps you, gentle reader, can be the change that the world needs here.

**Mitigating this anti-pattern:** Use the `generatorResults` property when
designing your generators, to ensure they behave as you expect. When they're
finely-tuned, add to your test-suite the `producesAllValues` property for every
generator you design.


### Showing

QuickCheck requires a `Show` instance for any values it generates when proving a property. Furthermore, while QuickCheck can lift any boolean into a
property, doing so results in test failures that don't come with any diagnostic
information whatsoever. In contrast, the `(===)` function compares two values
for equality, and informs you what its arguments were should it fail --- but
`(===)` also needs a `Show` instance. While this doesn't feel particularly
onerous, many exotic types don't immediately admit any obviously-meaningful
`Show` instances.

If your type doesn't have a meaningful `Show` instance, there are a few
workarounds. An orphan instance can be given in your test file, allowing tests
to `Show` your type, even if your public API's consumers are unable to. Orphan
instances are unanimously considered poor style, but by defining them only in
our test files, they cannot leak back and infect the main codebase.

But what should the `Show` instance look like for an unshowable type? There are
two solutions to this problem, with varying degrees of effort required. The
easiest path forwards is to steam-roll the problem by giving a useless instance:

```haskell
instance Show MyType where
  show _ = "<MyType>"
```

I strongly recommend using the pattern above for the displayed string: putting
the type name in angle-brackets. Such a thing is syntactically distinct from all other
values that can be shown and simultaneously alerts you to the type in question.
The steam-rolling approach is adequate for getting your test-suite up and
running quickly but is utterly useless during debugging. If your law is
false intermittently, knowing as much as possible about its generated values is
eminently helpful.

Rather than steam-rolling `Show` instances for *all* tricky types, a better
solution is to give it a best-effort attempt. Functions and existential types are the only truly
tricky things to show. While QuickCheck has some support for showing functions,
it is non-compositional and as good as useless for real-life tasks. An
acceptable workaround for functions is to give a test-local `INCOHERENT`
instance:

```haskell
instance {-# INCOHERENT #-} Show (a -> b) where
  show _ = "<fn>"
```

For types that are unshowable due to containing functions, the above instance is
sufficient for allowing you to derive their `Show` instances again.

Types that contain existentials are a more significant hurdle. Many people, stumbling on
this problem for the first time, will first attempt to pack a `Show` dictionary
for their existential type inside its data constructor. This does indeed
solve the problem, at the cost of leaking that test-requirement as part of the
public interface. Instead, a better solution is to simply hand-roll a `Show`
instance for the containing type, and use a placeholder for the existential.

```haskell
data HasExistential where
  Existential :: String -> t -> HasExistential

instance Show HasExistential where
  show (Existential s _) = unwords
    [ "Existential"
    , show s
    , "<exists t>"
    ]
```

This solution isn't very satisfying, but the utility of doing better simply
isn't worth the time or engineering effort.


### Shrinking

One of QuickCheck's killer features is *shrinking.* While it generates truly
random data for testing laws, the data it shows to the user when a test fails is
always a locally-simplest reproduction. In essence, QuickCheck will
automatically shrink a test case to a simpler reproduction. The canonical
example is checking whether a list is the same as its reverse (it shouldn't be,
in general):

```haskell
quickCheck $ \(xs :: [Int]) -> xs === reverse xs
```

```text
*** Failed! Falsified (after 3 tests and 1 shrink):
[0,1]
[0,1] /= [1,0]
```

The reproduction QuickCheck gives us is the list `[0,1]`, which, as it points
out, is not equal to its reverse. Interestingly, QuickCheck will almost always
give this example, but behind the scenes, it honestly is trying
arbitrarily complicated lists. When it finds a failing case, it runs the
shrinker, finding simpler lists that also fail the test. It continues this
way until it can't find a smaller term that fails the test, and then informs the
user of its findings.

QuickCheck's shrinking behavior is beneficial for debugging failures.
Notice in the output above, the counter-example is two elements long, with the
two "simplest" integers. Why isn't the list one element long? Because a
single-element list is its own reverse! Why aren't the two elements the same?
Because again, that list would be its own reverse. The presented counter-example
is hugely informative about the circumstances in which our property fails!

As great as shrinking is, it requires some effort on our part to make happen. In
particular, our `Arbitrary` instance must also define a `shrink` function; this
is not enabled by default.

```haskell
class Arbitrary a where
  arbitrary :: Gen a
  shrink    :: a -> [a]
```

The default behavior of `shrink` is not to shrink anything, and so QuickCheck
will stubbornly report remarkably complicated counter-examples until you implement
it for yourself. Thankfully, there is a generic implementation given by
`genericShrink`, which can be used as follows:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)

data MyType = ...
  deriving (Generic)

instance Arbitrary MyType where
  arbitrary = ...
  shrink    = genericShrink
```

This simple addition is sufficient to enable shrinking on your types. However,
there's a caveat, which is that `genericShrink` will not preserve data
invariants. For many data types, this is OK, but if you have an invariant, we can
reuse our `valid` function (for example, the `validCounter` that we wrote
above.) Because `valid` ensures our data invariant, we can simply filter out any
shrunk terms that are invalid:

```haskell
instance Arbitrary MyType where
  arbitrary = ...
  shrink    = filter valid . genericShrink
```

Adding shrinkers for your generators will pay for itself on the very first bug
you encounter. Since you need to test your generators anyway, why not reuse that
effort?


### Using QuickCheck Interactively

Most people think of unit tests as things that ensure your code is correct. But
when working with property tests, we can think about tests in the more
scientific sense. That is, we can make hypotheses about our implementation and
use property tests to confirm or deny those ideas. For example, returning
to our tile example from @sec:tiles, we might wonder if it's possible for small
renderings of tiles ever to misrepresent larger rasterizations of the same tile. That
is to say, are there ever colors that appear in very small rasters that don't
appear in large ones of the same tile?

Maybe this is less crazy than it sounds. Imagine we make a tile like @fig:lt
below. It's probable that different choices of width and height will sample
different parts of the image --- but will that choice be stable? Rather than
wondering, we can just write a property test that checks!

```{#fig:lt design=code/Tiles/Efficient.hs label="Example"}
rows [ color 0.99 0.73 0 1, color 0.99 0.73 0 1, color 0 0.42 0.27 1, color 0.76 0.15 0.18 1, color 0.76 0.15 0.18 1 ]
```

We decide to ask if there is any width and height such that multiplying them by
two changes the set of colors in the final tile.

```haskell
quickCheck $ \(t :: Tile Color) (Small w) (Small h) ->
  S.fromList (concat $ rasterize w h t)
    == S.fromList (concat $ rasterize (w * 2) (h * 2) t)
```

```text
*** Failed! Falsified (after 26 tests and 5 shrinks):
<tile>
Small {getSmall = 1}
Small {getSmall = 1}
```

And there are! After shrinking, QuickCheck has found a 1x1 tile that contains,
presumably, fewer colors than when doubled. It doesn't show us what that
tile is, but in retrospect, this answer is obvious --- two solid colors
`beside` one another must exhibit this behavior!

Although we used QuickCheck interactively in this rather silly example, the
approach is particularly well suited for determining if invariants
always hold in particular constructions. It might not be the case that our laws
state something must be true, but it could be a byproduct of the implementation.
If so, it's much easier to convince ourselves with a property test than to give a proof.

