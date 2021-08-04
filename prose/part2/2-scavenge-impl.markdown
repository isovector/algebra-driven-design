## Scavenger Hunt Implementation {#sec:scavenge-impl}

Now that we have seen how to derive implementations for our simple `Tile`
algebra, let's use the same technique on a more challenging example. We will
return to our scavenger hunt system. Recall, in its final form we had
generalized away all of the concrete inputs, filters, rewards and clues, leaving
behind only the constructors:

```haskell
data Challenge i k r
data InputFilter i

empty  :: Challenge i k r
reward :: r -> Challenge i k r

gate
    :: InputFilter i
    -> Challenge i k r
    -> Challenge i k r

andThen
    :: Challenge i k r
    -> Challenge i k r
    -> Challenge i k r
both
    :: Challenge i k r
    -> Challenge i k r
    -> Challenge i k r
eitherC
    :: Challenge i k r
    -> Challenge i k r
    -> Challenge i k r

bottom :: Challenge i k r

clue :: [k] -> Challenge i k r -> Challenge i k r
```

as well as a handful of observations:

```haskell
data ClueState

seen      :: ClueState
completed :: ClueState
failed    :: ClueState

completes  :: Challenge i k r -> [i] -> Bool
getRewards :: Challenge i k r -> [i] -> r
getClues
    :: Challenge i k r
    -> [i]
    -> MonoidalMap [k] ClueState
```

Also, we have a sort of "sub-algebra" corresponding to the land of user
inputs and predicates over that input:

```haskell
always :: InputFilter i
never  :: InputFilter i

andF :: InputFilter i -> InputFilter i -> InputFilter i
orF  :: InputFilter i -> InputFilter i -> InputFilter i
notF :: InputFilter i -> InputFilter i

custom :: HasFilter i => CustomFilter i -> InputFilter i

matches :: HasFilter i => InputFilter i -> i -> Bool
```

Altogether, this algebra is significantly more complicated than the tiles in
@sec:tiles-impl. How well does Algebra-Driven Design stack up here?

As before, we will first construct an initial encoding of our algebra --- not
because it's necessarily an efficient solution, but because it's easy to get right. We
will then use QuickSpec to generate additional equations about this
implementation, and immortalize them in a suite of regression-focused property
tests. And finally, armed with a comprehensive test suite to ensure we don't
accidentally introduce bugs or change the semantics, we will tease out a fast
and elegant implementation.


### The Filter Algebra

Rather than diving into the `Challenge` algebra directly, let's first implement
the machinery for input filters, as it is simple, standalone, and necessary for
the implementation of challenges.

Input filters form a basic boolean algebra, meaning all of our familiar
optimizations and manipulations are in play. But those optimizations can only
possibly serve to obscure what we're trying to do, so let's encode the input
filter mechanically and naively. We begin with the data type, giving one
primitive form for each constructor in the `InputFilter` sub-algebra:

[code/Scavenge/InputFilter.hs:InputFilter](Snip)

The implementations of our constructors are extremely straightforward, making no
attempt whatsoever to simplify the expressions:

[code/Scavenge/InputFilter.hs:always](Snip)

[code/Scavenge/InputFilter.hs:never](Snip)

[code/Scavenge/InputFilter.hs:andF](Snip)

[code/Scavenge/InputFilter.hs:orF](Snip)

[code/Scavenge/InputFilter.hs:notF](Snip)

[code/Scavenge/InputFilter.hs:custom](Snip)

Input filters are observed via `matches`, which is a straightforward
homomorphism into boolean predicates:

[code/Scavenge/InputFilter.hs:matches](Snip)

Our next step is always to write a generator for the types in question. As
usual, our generator checks the desired complexity and chooses a terminal
constructor if a simple term is requested. Otherwise, it picks any constructor,
giving preference to the user-defined `custom` filters:

[code/Scavenge/InputFilter.hs:ArbitraryInputFilter](Snip)

To truly kick this algebra's tires, we will need a user-defined input type.
Rather than building the entire input system described in @sec:scavenge-design, we will
create something much more straightforward. Inputs will be of the form `Number Word8`, and
there will be a single custom filter `exactly :: Word8 -> InputFilter Test`.
This is enough to simulate what our library users will do, without requiring
a significant amount of investment on our part.  In a real library, we would
likely provide reusable testing components so that our users could derive the
majority of their test infrastructure from ours --- though that would distract
from the point here.

The `Test` type defines our custom input filters:

[code/Scavenge/InputFilter.hs:Test](Snip)

and by giving it an instance of `HasFilter` we can define the inputs it matches:

[code/Scavenge/InputFilter.hs:HasFilterTest](Snip)

Finally, we can provide a constructor `exactly` that builds an `InputFilter Test`
directly:

[code/Scavenge/InputFilter.hs:exactly](Snip)

By defining `exactly` in this way we can expose it directly to `Challenge`
designers working over the test input --- they will never know it isn't a part
of the "core" library. Both `Test` and `Input Test` require arbitrary instances:

[code/Scavenge/InputFilter.hs:ArbitraryTest](Snip)

[code/Scavenge/InputFilter.hs:ArbitraryInputTest](Snip)

All that's left is to write some QuickSpec signatures and marvel at our
handiwork. The first of which is `sig_filter_cons`, which lists the core input
filters:

[code/Scavenge/InputFilter.hs:sig_filter_cons](Snip)

The `bools` signature at [1](Ann) is provided by QuickSpec and contains the
usual boolean algebra --- things like `True`, `False`, `(&&)`, `not`, and so on.
We will also provide a signature for our custom input filters:

[code/Scavenge/InputFilter.hs:sig_filter_user_cons](Snip)

Lastly, we need to declare all of the types in our filter sub-algebra:

[code/Scavenge/InputFilter.hs:sig_filter_types](Snip)

None of this should be shocking; after you've gone through the work of
building a generator and signature once, the second time doesn't provide any
additional challenges. After gluing all of our filter signatures together, we
can test the sub-algebra:

[code/Scavenge/InputFilter.hs:sig_filters](Snip)

```{#filter_laws quickspec=code/Scavenge/InputFilter.hs}
quickSpec sig_filters
```

Everything looks good here. The laws
[CiteLaw:filter_laws](matches always i),
[CiteLaw:filter_laws](matches never i = False),
[CiteLaw:filter_laws](matches (notF x) i = not (matches x i)),
[CiteLaw:filter_laws](matches x i && matches y i = matches (andF x y) i), and
[CiteLaw:filter_laws](matches x i || matches y i = matches (orF x y) i) are the
boolean homomorphisms we expect, showing that our input filter algebra really
is just a filter over inputs. In addition, law
[CiteLaw:filter_laws](matches (exactly n) (Number n)), shows that `exactly`
matches a `Number` input whenever the two numbers are the same.

Not bad. Let's now tackle the hard part.


### The Challenge Algebra

Our first step is to try to minimize the number of primitive forms in the
`Challenge` algebra. Law `law:bottom` is an obvious candidate, giving us:

[code/Scavenge/Initial.hs:bottom](Snip)

One particular annoyance when working with initial encodings is having two
terminal constructors which behave similarly --- in our case `empty` and
`reward` both immediately reduce to `empty`, but all of our `step` laws are in
terms of `isEmpty`. Rather than dealing with double logic everywhere, we can
instead use `law:andThen/empty` to rewrite `reward r` as `andThen (reward r)
empty`. In this form, `reward r` no longer acts as a terminal constructor, so
we can use `andThen (reward r)` as a primitive form:

```{.haskell law="RewardThen"}
∀ (r :: r) (c :: Challenge i k r).
  RewardThen r c
    = andThen (reward r) c
```

which we can use to give an implementation for `reward` as follows:


```{.haskell .proof}
  reward r
=  -- .via andThen/empty
  andThen (reward r) empty
=  -- .via RewardThen
  RewardThen r empty
```

Using `RewardThen` as a primitive instead of the more-direct `Reward` is
valuable to prevent our otherwise doubled-logic, and comes with an added benefit
--- it sets things up such that we can expect `law:andThen/reward/reward` to
fire often:

```{.haskell law="law:andThen/reward/reward"}
∀ (r1 :: r) (r2 :: r).
  andThen (reward r1) (reward r2)
    = reward (r1 <> r2)
```

We will map our remaining constructors to primitive form data constructors:

[code/Scavenge/Initial.hs:Challenge](Snip)

The construction of terms is straightforward; again, each of our laws relating
constructors to one another (that is to say, laws without any observations)
corresponds to a pattern match in the implementation for the constructor. Empty
and gate are both elementary:

[code/Scavenge/Initial.hs:empty](Snip)

[code/Scavenge/Initial.hs:gate](Snip)

As we saw above, `reward` can be rewritten in terms of `rewardThen`:

[code/Scavenge/Initial.hs:reward](Snip)

while `rewardThen` itself is responsible for ensuring `law:reward/mempty` and
`law:andThen/reward/reward` both hold:

[code/Scavenge/Initial.hs:rewardThen](Snip)

Implementing `andThen`, `both` and `eitherC` are also easy. All laws for these
forms are perfectly confluent, meaning it doesn't matter in which order we
expand them, or in what order our pattern matches occur --- although we must be
careful to "bubble up" any `rewardThen` nodes we discover.

[code/Scavenge/Initial.hs:andThen](Snip)

[code/Scavenge/Initial.hs:both](Snip)

[code/Scavenge/Initial.hs:eitherC](Snip)

Finally, we come to our clue, which is also exceptionally simple:

[code/Scavenge/Initial.hs:clue](Snip)

We will also need to give a definition and implementations for `ClueState`:

[code/Scavenge/ClueState.hs:ClueState](Snip)

At [1](Ann), we are careful to list the primitive forms in the same order as the
desirability ordering discussed in @sec:scavenge-design. This allows us to have
our `Semigroup` and `Monoid` instances automatically generated at [2](Ann) using
the same ordering. The implementations for `ClueState` constructors are
exceptionally straightforward:

[code/Scavenge/ClueState.hs:seen](Snip)

[code/Scavenge/ClueState.hs:failed](Snip)

[code/Scavenge/ClueState.hs:completed](Snip)

While we're at it, let's make a helper data structure to hold the rewards
and clues that we'd like to emit.

[code/Scavenge/Results.hs:Results](Snip)

With all of our constructors hammered out, we can turn our attention to the
observations --- starting with our denotation, `step`. The easy cases are for
`Empty` and `Both`:

[code/Scavenge/Initial.hs:stepEmpty](Snip)

[code/Scavenge/Initial.hs:stepBoth](Snip)

Giving a step function for `RewardThen` requires us to emit the rewards and then
to step the child node:

[code/Scavenge/Initial.hs:stepRewardThen](Snip)

where `tellReward` is defined as:

[code/Scavenge/Initial.hs:tellReward](Snip)

Implementing `step` for `Gate` requires us to have an actual input (not a
`Nothing`), and for that input to match the filter. If it does, we step the
inner `Challenge` without any input. If there was no input, or if the filter
doesn't match, we don't step at all.

[code/Scavenge/Initial.hs:stepGate](Snip)

Similarly, to step an `AndThen` node we must step the first argument,
and step the second only if the first stepped to `Empty`. If it didn't, we
rebuild the `andThen` node with the stepped first child:

[code/Scavenge/Initial.hs:stepAndThen](Snip)

`EitherC` proves a more difficult beast (recall the horror of `law:step/eitherC
empty`.) We must step both children and then check if either results in
`Empty`, pruning the other. Again, if neither steps to `Empty`, we rebuild the
`eitherC` node with its newly stepped children:

[code/Scavenge/Initial.hs:stepEitherC](Snip)

The helper function `prune` is given after the conclusion of `step`, as it's
rather involved.

Finally, we come to the `Clue` constructor. Here, recall, we must update the clue
context, and step the child with it. If the child steps to `Empty`, the clue is
completed, and this node also steps to `Empty`. Otherwise we mark the clue as
seen and preserve the `clue` node:

[code/Scavenge/Initial.hs:stepClue](Snip)

Stepping `Clue` also requires a helper function to emit clue states, given by:

[code/Scavenge/Initial.hs:tellClue](Snip)

All that's left is the implementation of the helper function `prune`, which
discovers all of the `seen` clues and emits them as `failed` clues instead.

[code/Scavenge/Initial.hs:prune](Snip)

As you can see, `prune` is implemented in terms of `findClues`, which comes
directly from `law:findClues` --- and in fact, was given as a function there.

[code/Scavenge/Initial.hs:findClues](Snip)

Easy as that. Our observations, too, come immediately from the laws ---
`pumpChallenge` from `law:pumpChallenge`:

[code/Scavenge/Initial.hs:pumpChallenge](Snip)

and its specializations to more specific observations are likewise entirely
law-driven. In fact, each of them has a significantly longer type signature than
implementation!

[code/Scavenge/Initial.hs:runChallenge](Snip)

[code/Scavenge/Initial.hs:getRewards](Snip)

[code/Scavenge/Initial.hs:getClues](Snip)


### Testing It

To glue everything together, we need to give appropriate instances of
`Arbitrary` and `Observe` for `Challenge`. The former instance looks exactly as
you would expect:

[code/Scavenge/Initial.hs:ArbitraryChallenge](Snip)

and our observational equality is nothing other than `runChallenge`:

[code/Scavenge/Initial.hs:ObserveChallenge](Snip)

At long last, we have all of the machinery out of the way. We'd like to test all
of our hard work, but have one final hurdle to tackle --- everything is
polymorphic! What is a great boon when working through the design is problematic
when executing it; at the time of writing, no tool will do property
testing by randomly instantiating type variables.[^maybe-there-should]

[^maybe-there-should]: But it's an intriguing idea. Maybe someone should try
  it.

Thankfully, naturality comes to the rescue; by being universally
quantified, our code isn't allowed to *branch* on its types. The only parts of
the implementation that might not generalize are the type classes --- which might
seem like a lot of surface area when you consider all of the constraints on
`pumpChallenge`:

```haskell
  ( Ord k
  , HasFilter i
  , Monoid r, Commutative r, Eq r
  )
```

Of our five constraints, two are for equality, and one is for ordering ---
neither of which would plausibly affect the results of a test. Two of the
remaining constraints pertain to monoids: requiring that `r` be a commutative
monoid. As we've seen, there are many possible monoids, but again, the
quantifier of universality means we must *only* be using these types' monoidal properties. Anything else simply wouldn't type check.

The `HasFilter i` constraint is all that's left, but we as implementers know
it's used only by `matches`, and thus shouldn't affect our `Challenge`
algebra at large in any meaningful way.

But the question remains: how should we instantiate these type variables for our
test? It helps to step back and ask what we're worried about --- and in this
case, we're concerned that any particular type might have more equal terms than
are strictly necessary. If our stochastic test hits these extra equalities, they
might "leak upwards" into untrue laws about challenges.

When presented like this, the solution crystallizes: we should pick types that
have no extraneous equalities. Rather than trying to conjure such types up
ourselves, we can turn to the theory. The types we're looking for are known as
"free," for example we'd like to use a *free monoid* for our clues and a *free
commutative monoid* for the rewards. A free construction is one in which the
mandatory laws hold, but in which *absolutely no other laws* hold. The free
monoid is thus, in some sense, the "least interesting" monoid.

Further discussion of free constructions is outside the scope of this book, but
the rule of thumb is to always instantiate a type variable with a free
construction, if possible.

The free monoid is a list, and the free commutative monoid is a
multiset.[^idempotent-set] But lists and multisets of what? At this point, it's
our choice, so we might as well choose integers.

[^idempotent-set]: Regular sets appear as the free *idempotent* commutative
  monoid.

[code/Scavenge/Test.hs:TestReward](Snip)

[code/Scavenge/Test.hs:TestClue](Snip)

We can now proceed to write a QuickSpec signature for challenges. First, let's
add the constructors:

[code/Scavenge/App.hs:sig_cons](Snip)

and the types:

[code/Scavenge/App.hs:sig_types](Snip)

The critical piece of `sig_types` is marked by [1](Ann) --- QuickSpec doesn't
know that lists or rewards are monoids unless we tell it! This is true of every
typeclass: any relevant to the algebra must be explicitly added to a
signature.

QuickSpec doesn't know anything about monoids either. By
adding them to a background, we can let it independently discover the monoid
laws before getting starting on our algebra.

[code/Scavenge/App.hs:sig_monoid](Snip)

We use `liftC` to tell QuickSpec that the monoid operations are valid for any
type with a monoid instance. But again, the curse of testing polymorphism
strikes, and QuickSpec won't discover the monoid laws unless its *default type*
has a monoid instance. Using the `defaultTo` operator, we can change this
default, as marked at [1](Ann).

Finally, we need to set which options we'd like for QuickSpec:

[code/Scavenge/App.hs:sig_opts](Snip)

Because our algebra consists of a bunch of inductive constructors, all of which
consume and produce the same type, you can imagine there are *lots* of different
ways of gluing them together. By setting `variableUse Linear` like at [1](Ann)
we can tell QuickSpec to only use a given variable once per side of an equation
--- drastically cutting down on the weird laws it presents to us. This is not
mandatory, but without it, QuickSpec will find several properties about
challenges that no human would ever write. Such output is not useless --- this
option should be removed when generating regression tests ---  but it is noisy
and can get in the way of human understanding.

All that's left is to connect all of our signatures:

[code/Scavenge/App.hs:sig](Snip)

and we can finally see what properties hold for our implementation:

```{#challenge_laws quickspec=code/Scavenge/App.hs}
quickSpec $ sig <> sig_opts
```

Everything looks great! Let's turn off the linearity requirement and bump up the
number of successes before generating our property tests:

[code/Scavenge/App.hs:sig_test_opts](Snip)

Now, running the following command will generate the high-confidence regression
tests to nail down the semantics as we've implemented them here. Hit the return
key and let QuickSpec churn overnight.

```haskell
quickSpec $ sig <> sig_test_opts
```


### Implementation {#sec:cps}

The property tests generated in the previous section now form the acceptance
criteria for our performant, final implementation; we're golden when all of the
tests go green. At that point, the semantics captured by our initial encoding
are faithfully represented: meaning we didn't break anything while optimizing.

Sometimes the semantic equations are advantageous in finding an
efficient implementation, but unfortunately, this is not such a case. For
a particularly compelling example in which they are, see @hughes_design_1995.
Instead, we must make do with our wits and take solace that we now know as much
as we ever will about our design space.

It's helpful to ask ourselves where the inefficiencies are? How much work must
we do in the asymptotically optimal case? Clearly, in the worst case, we will need
to visit every node of the entire `Challenge` tree --- but importantly, there is
no reason to visit any node more than once.

In some sense, the only "interesting" constructor of our algebra is `gate`,
because, without it, every other tree would step immediately to `empty`.
Therefore, it's the effort required to get to the `gate` nodes that dominates
our performance.

Our model implementation requires traversing the entire tree to find the
incomplete nodes, propagating events to them, and then rippling their results
backward. For an extreme example, consider [@fig:bingoh;@fig:bingov] where we
partition a city into a game of "bingo," with a point of interest in each
square.

![Bingo columns](images/bingov.png){#fig:bingov}

![Bingo rows](images/bingoh.png){#fig:bingoh}

Players are required to complete every point-of-interest challenge within a
given row or column --- but every individual challenge counts as progress
towards both a row and column, as in @fig:bingovh.

![Every challenge is part of both a row and a column.](images/bingovh.png){#fig:bingovh}

We can represent this scenario as a big "sum of products." Let's say our
top-left-most challenge is `c11 :: Challenge i k r`, the one to its right is
`c21 :: Challenge i k r` and the one below `c11` is `c12 :: Challenge i k r`.
Then our left-most column is:

```haskell
col1 :: Challenge i k r
col1 = foldr both c11 [c12, c13]
```

and the bottom-most row is:

```haskell
row3 :: Challenge i k r
row3 = foldr both c31 [c32, c33, c34, c35, c36, c37, c38]
```

Each of `colN` and `rowN` corresponds to a challenge in which the player must
complete an entire row or column. And now to allow the player to work on the
entire grid simultaneously, we must combine every row and column via `eitherC`:

```haskell
grid :: Challenge i k r
grid =
  foldr eitherC col1
    [ col2, col3, col4, col5, col6, col7, col8
    , row1, row2, row3
    ]
```

The resulting `Challenge` tree for `grid` has 24 points of interest --- which
are also the *nodes* of interest --- but requires 37 `both` nodes and 9
`eitherC` to glue everything together. It must be
propagated through this entire tree for every incoming input, just to be routed to the relevant `gate`.
In general, a challenge with $n$ gates and $i$ inputs will require $O(i\times
n)$ work just to route inputs.

As you can see, we could shave off a substantive amount of this work if we could
somehow flatten out the tree; keeping track of only which gates are currently
accessible. Similar to our distribution laws that bubbled `reward` nodes
upwards, we can intuitively imagine some sort of process that bubbles up
knowledge of available gates. There is excellent potential for speedup here: for
every incoming input, we could then check it against only the relevant filters
--- shaving off a factor of $O(n)$ in the process. This is the insight, but
making it happen is quite another thing.

The other insight here is that it would be enough to simply eliminate the tree
structure in our representation of `Challenge`. We can't spend time traversing a
tree that doesn't exist!

Let's try putting these two ideas together. At any given point in time, a
`Challenge` has some associated state corresponding to which gates are currently
gated, which rewards have been given, and which clues are in which state. We
will find it convenient also to keep track of whether or not the entire
challenge has been completed --- corresponding to the result of `runChallenge`.
As a first attempt, we can build it as:

```haskell
data ChallengeData i k r = ChallengeData
  { waitingOn
      :: MonoidalMap
           (InputFilter i)
           (ChallengeData i k r)  -- ! 1
  , results :: Results k r
  , isComplete :: Bool
  }
```

The `waitingOn` field here is odd and requires some explanation. For every gate,
we store its corresponding `InputFilter` as the key to our map. Rather than
packing its value with a `Challenge` as the `gate` constructor does, the
observation is that getting through a gate is only going to do three
possible things. First, it might award us some rewards. Second, it could change
the state of some clues. And third, it might make more gates accessible. Each of
these things can be represented by a `ChallengeData`, thus explaining
why it appears at [1](Ann).

Life will be easier if we give a `Monoid` instance for `ChallengeData`. A record
is nothing more than a collection of things, and so we can give a trivial
monoid for it by lifting its fields' monoids point-wise. Because there are two
different monoids over booleans (the `and` and the `or` monoids, respectively,)
we must choose one. Combining a finished challenge with an unfinished one seems
like it should finish the entire thing, so we pick the `or` monoid over
booleans --- which is called `Any` in Haskell:

```haskell
data ChallengeData i k r = ChallengeData
  { waitingOn
      :: MonoidalMap
           (InputFilter i)
           (ChallengeData i k r)  -- ! 1
  , results :: Results k r
  , isComplete :: Any
  }
```

The monoid formed by the product of monoids is such a common algebraic component
that we can ask Haskell to write the code for us, using `Generic.Data` from
the `generic-data` library:

[code/Scavenge/CPS.hs:SemigroupCData](Snip)

[code/Scavenge/CPS.hs:MonoidCData](Snip)

If we imagine that the implementation of `Challenge i k r` is simply
`ChallengeData i k r`, it's easy to imagine an implementation for `reward`:

```haskell
reward :: r -> ChallengeData i k r
reward r = mempty { results = Results r mempty }
```

Devilishly delicious!

But `ChallengeData` is not quite the entire solution. It can't represent the
sequential behavior of `andThen`, the two-success behavior of `both`, or the
pruning behavior of `eitherC`. As disparate as these things seem, they are all
special cases of "what should happen next," or *continuations.* In the so-called
*continuation-passing style*, program components explicitly pass around a
continuation, decorating it as need be. The original caller gives a "reasonable"
continuation, which in our case should set the `isComplete` field to `Any True`.

Consider the term `andThen c1 c2`. The `andThen` constructor can intercept its
incoming continuation and hold onto it. Rather than giving it to `c1`, it will
provide `c1` a continuation that says, "when you're finished, run `c2`." The
original continuation is then handed instead to `c2`. This is always a
mind-boggling concept the first few times one tackles it; perhaps a code example
will help.

Let's pretend that our implementation of `Challenge` is this:

```haskell
type Challenge i k r
    = ChallengeData i k r  -- ! 1
   -> ChallengeData i k r
```

where [1](Ann) marks the continuation. We can then implement `andThen` as
follows. The type synonym of `Challenge` here has been expanded to help the
casual reader with mentally type checking, but shouldn't be expanded if you're
coding along.

```haskell
andThen
    :: (ChallengeData i k r -> ChallengeData i k r)
    -> (ChallengeData i k r -> ChallengeData i k r)
    -> ChallengeData i k r
    -> ChallengeData i k r
andThen c1 c2 cont = c1 (c2 cont)
```

The continuation (`ChallengeData i k r` in this case) that we give to `c1` is in
fact `c2 cont`. We can describe this in English as "do `c1`, and when you're
finished, do `c2`. Only after you're finished `c2` should we do `cont`: the
thing next asked of us."

We can try the same approach to implement `both`, but we would get stuck
instantly. The two sub-challenges of `both` are supposed to run simultaneously,
so we can't do this same trick. Instead, we'd like to construct a new
continuation that does nothing the first time it is called (which would the
first child finishing), and only on its second call should it invoke the
original continuation. Such a thing would need to be stateful, and
thus so too must our entire continuation machinery.

In Haskell, we can use the `ST` monad to get access to local state. Specific
understanding of the `ST` machinery is outside the scope of this book, but at a
high level, it allows us to create, read, and write variables. The only catch is
that the variables are not allowed to leave the scope of the computation,
ensuring that this statefulness can't leak out and infest the rest of the
idyllic and pure Haskell that we know and love.

Implementing `both` is now possible, using the aforementioned technique. With
a new pretend implementation for `Challenge` that runs in the `ST s` monad:

```haskell
type Challenge i k r s
    = ST s (ChallengeData i k r s)
   -> ST s (ChallengeData i k r s)
```

which is now capable of using state locally, we can implement `both`:

```haskell
both
    :: (ST s (ChallengeData i k r)
          -> ST s (ChallengeData i k r))
    -> (ST s (ChallengeData i k r)
          -> ST s (ChallengeData i k r))
    -> ST s (ChallengeData i k r)
    -> ST s (ChallengeData i k r)
both c1 c2 cont = do
  remaining  <- newSTRef 2  -- ! 1
  let cont' = do  -- ! 2
        modifySTRef' remaining $ subtract 1
        readSTRef remaining >>= \case
          0 -> cont  -- ! 3
          _ -> pure mempty
    d1 <- c1 cont' -- ! 4
    d2 <- c2 cont'
    pure $ d1 <> d2  -- ! 5
```

At [1](Ann), we create a new mutable variable containing the number of
sub-challenges that still need to be completed. We construct a new continuation
([2](Ann)) which decrements the `remaining` variable, and calls the old
continuation ([3](Ann)) if `remaining` is now set to zero. We can now run the
sub-challenges with the "decorated" continuation at [4](Ann), and monoidally
combine their resulting `ChallengeData` together as the final result of `both`.

Let's walk through an example of this together. Recall that our initial
continuation is the "reasonable" one, which sets the `isComplete` field of
`ChallengeData`.  Let's give it a name:

```haskell
end :: ST s (ChallengeData i k r)
end = pure $ mempty { isComplete = Any True }
```

Take the term `andThen (both (reward r1) (reward r2)) (reward r3)`. If this is
the root of the tree, it will have been given continuation `end`, and thus our
task is to evaluate:

```haskell
andThen (both (reward r1) (reward r2)) (reward r3) end
```

By the definition of `andThen`, the continuation gets moved:

```haskell
both (reward r1) (reward r2) (reward r3 end)
```

Now `both` allocates `remaining = 2`, and calls `reward r1` with the decorated
continuation; written in pseudo-code as:

```haskell
reward r1 <decorated continuation>
```

which emits reward `r1`, and then calls its continuation --- the decorated one.
The continuation sets `remaining = 1`, and then returns `mempty` because
`remaining` is not yet zero.  This branch of `both` has ended, so we move on to `reward r2`.

The `reward r2` also emits its reward and calls its continuation. Here we set
`remaining = 0` and the original continuation to `both` (which was `reward r3
end`) is now called. Just like last time, now the `reward r3` challenge gives
its reward, and then calls its continuation, which is finally `end`. All of the
resultant `ChallengeData` are appended together, giving us the three rewards and
`isCompleted = Any True`.

It's all a bit convoluted, certainly, but it works --- and most importantly, it
has eliminated the tree structure of `Challenge`. Each of our
constructors does some work to manipulate the continuations of its children,
but crucially, *it only does it once.* The desired computation is now embedded
entirely within the braiding of continuations and `ChallengeData`.

By sticking the `ST s` monad into the value of `waitingOn`, we can finalize the
definition of `ChallengeData`:

[code/Scavenge/CPS.hs:ChallengeData](Snip)

Our `gate` constructor can be implemented using the same continuation idea. But
instead of running our continuation immediately, we can give it to the
`Challenge` that is supposed to be run after the gate is opened:

```haskell
gate
    :: InputFilter i
    -> (ST s (ChallengeData i k r)
          -> ST s (ChallengeData i k r))
    -> ST s (ChallengeData i k r)
    -> ST s (ChallengeData i k r)
gate f c cont =
  pure $ mempty
    { waitingOn = M.singleton f $ c cont }
```

While we're knocking out easy constructors, let's also tackle `bottom`. Because
`bottom` never completes, it can never call its continuation, and so its
implementation can just drop the continuation entirely:

```haskell
bottom
    :: ST s (ChallengeData i k r)
    -> ST s (ChallengeData i k r)
bottom _ = mempty
```

And we can give an implementation for `empty`. Recall that `empty` is our only
"true" terminal constructor used to mark the end of a challenge. And at
the end of a challenge, we should do what? That's right --- simply call the
continuation!

```haskell
empty
    :: ST s (ChallengeData i k r)
    -> ST s (ChallengeData i k r)
empty cont = pure cont
```

All that remains now are `clue` and `eitherC`, which happen to be riddled with
subtle complexities. It took me an entire day to get these two
implementations correct in full disclosure, but that was no matter; I had over 80 property tests
(corresponding to 800 unit tests!) to guide me. Many things I tried would
fail only one or two tests, and the resulting bugs certainly never would have been caught manually.

To emit clues with the correct "preamble," our old friend, the clue
context, must return as a parameter to every `Challenge`. Rather than using a
list, however, we will use a `DList`, which supports constant-time `snoc`-ing.
Additionally, we will also pass around a function `[k] -> ClueState -> ST s
ClueState` which serves to transform a clue state if necessary, and will be
motivated later.

Instead of managing these parameters individually, let's construct
a new type to do the work for us. This is the final implementation for
`Challenge`:

[code/Scavenge/CPS.hs:Challenge](Snip)

The `forall s` quantifier at [1](Ann) allows us to hide the `s` parameter that
is an implementation detail of the `ST s` monad. With the necessary exception of
constraints on types, no other implementation details should leak out of our
public interface.

Challenges also admit a monoid instance where the multiplication operation
performs all of the stateful operations in both branches before combining their
results monoidally:

[code/Scavenge/CPS.hs:SemigroupChallenge](Snip)

[code/Scavenge/CPS.hs:MonoidChallenge](Snip)

To implement `clue` we must compute the full "name" of the clue. We will then
transform the clue state from `seen` using our yet-unmotivated function. The
sub-challenge will then be run, decorating its continuation with one that will mark
the clue as completed. Finally, we must emit the clue in the state after its
original transformation.

[code/Scavenge/CPS.hs:clue](Snip)

where `tellClue` is a helper function given in a moment. At [2](Ann) we
use the unmotivated `rec` function to transform the state of our clue from
`seen` --- although nothing prevents `rec` from giving us `seen` back. The block
at [3](Ann) consists of the decorated continuation.

Notice the opportunity for optimization at [1](Ann). The implementation of
`clue` is to create a new continuation for every element in the clue list, but
all of these are always guaranteed to have the same value. We could eliminate a
linear amount of work here again by packing all of these clues together into a
single continuation, but the details of this are tricky and irrelevant for the
purposes of this book.


Exercise

:  Implement the optimization of dealing with the entire clue list in a single
   continuation.


[code/Scavenge/CPS.hs:tellClue](Snip)

Finally, we come to `eitherC`, which is uniquely complicated due to its pruning
behavior. As soon as one of its sub-challenges calls its continuation, we must
fail all of the other sub-challenge's clues. To do so, we
clearly must track which clues are emitted from which branch --- which finally
motivates our mysterious `rec :: DList k -> ClueState -> ST s ClueState`
function. We can decorate it to record clues coming out of a particular
branch. This is why the implementation of `clue` calls `rec`.

Each branch can decorate
its continuation to immediately fail its sibling's clues by recording every clue emitted in the other. This brings up a
second difficulty: the semantics of `eitherC` are that its two sub-challenges
are traversed simultaneously. But this is not yet true in our
implementation!

Because of this, the first sub-challenge might run its continuation and attempt
to prune the second sub-challenge's clues *before the second sub-challenge has
run at all!* In this case, the first child won't prune the second's
clues, because it doesn't yet know what they are. The trick is to make the
pruning action also cause the other branch's *newly-emitted clues* to be failed
immediately --- motivating why the `rec` function can change the
`ClueState` of an emitted clue.

A third difficulty: we must make sure that the continuation to `eitherC` is
called exactly once. Failure to enforce this can wreak all sorts of havoc, like
inadvertently reducing `andThen (both bottom (eitherC c1 c2)) c3` to `c3` ---
even though it should never be able to get past the `bottom`. To solve
this problem, we introduce a small combinator that uses a boolean variable to
make sure an `ST` action is run exactly once:

[code/Scavenge/CPS.hs:oneshot](Snip)

We can write a second helper function that reads a variable of clues and emits
each of them in the `failed` state:

[code/Scavenge/CPS.hs:prune](Snip)

One last helper is required; this one to decorate our `rec` call:

[code/Scavenge/CPS.hs:decorate](Snip)

Here we check to see if the other sub-challenge has already completed
([1](Ann)), and if so, immediately emit the clue as `failed` ([2](Ann)).
Otherwise, we insert it into the mutable set of clues we've encountered in this
branch ([3](Ann)) before calling `rec` function with the original clue state.

Finally, we're ready to implement `eitherC`.

[code/Scavenge/CPS.hs:eitherC](Snip)

As discussed above, we first make a variable for if either branch has called its
continuation ([1](Ann)), and two more variables for the clues we find in either
branch ([2](Ann)). We then call each sub-challenge, decorating their `rec`
function with `decorate` as in [3](Ann). Each branch has a `oneshot`
continuation that calls the original continuation and prunes the clues
discovered in the branch ([4](Ann)).

We're not quite done, but are getting very close. We still have the observations
to implement.  Like in the initial encoding, we give a `step` function which
checks an input against the currently waiting gates, and merges the resulting
`ChallengeData` if it matches:

[code/Scavenge/CPS.hs:step](Snip)

First, at [1](Ann) `step` gets the input filters currently being waited for. It
then traverses them ([2](Ann)), checking at ([3](Ann)) for any which match the
current input. If they do, it gets the resulting `ChallengeData` at [4](Ann),
merging it and pends a delete for the now-completed input filter ([5](Ann)).
These deletions are collected using the monoid over endomorphisms (functions
from a type to itself.) We must be sure to delete the completed filters before
merging in the new ones --- just in case the new filters are waiting on exactly
the same filters. This logic performed at [6](Ann) finally merges the
newly discovered `ChallengeData` at [7](Ann).

The sheer amount of work we're getting out of monoids in `step` is nearly
overwhelming. We monoidally merge newly discovered gates, updated clue states,
new rewards, and delete filters all in this one function. As tricky as it is,
imagine having to write `step` while merging all of that data manually!

For whatever reason, `foldMapM` isn't a standardly available combinator, so we
can implement it for ourselves:

[code/Scavenge/CPS.hs:foldMapM](Snip)

We're nearing the end now. We'll need an implementation of `pumpChallenge` which
lifts `step` over multiple inputs:

[code/Scavenge/CPS.hs:pumpChallenge](Snip)

Here we must be cautious at [1](Ann) to make sure that we stop stepping a
challenge, which is complete. Otherwise, clues and rewards which should have been
pruned off might still leak out.

Finally, we can give the user-facing observation `runChallenge`:

[code/Scavenge/CPS.hs:runChallenge](Snip)

Filling in the clue context at [1](Ann), the undecorated clue recording function
at [2](Ann), and the "complete the challenge" continuation at [3](Ann).

Believe it or not; we're all done! Copy and paste the `Arbitrary` and `Observe`
instances for `Challenge`, run those property tests, and bask in the green results.

What an ordeal! But our efforts are for a worthy cause; we've successfully
removed the entire tree structure from `Challenge`. The result is a tangled mess
of stateful code that a human would be unlikely to get right if written by hand.
Thankfully, the algebra frees our users from having to write this code manually
--- rather than thinking about the stateful underpinnings of what they'd like to
do, they can instead work at the level of meaningful, compositional trees. We
have successfully implemented an abstraction that *cannot leak.*

