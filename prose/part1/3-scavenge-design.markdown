## Scavenger Hunt {#sec:scavenge-design}

Tiles are an excellent introduction to Algebra-Driven Design because they are easy to
visualize and intuitively deal with. Let's now turn our focus to a
domain that is more "real world," one without pre-existing mathematical
literature.

The system described in this section has been derived from actual software I wrote
in a professional capacity. However, despite four rewrites, we never once came
up with a suitable design that simultaneously met all of the requirements and
was easy to use. Of course, we weren't using Algebra-Driven Design when tackling
the design work. That is to say, this is a real-world case study of building
software where all other methodologies failed.

This time around, we will build the backend service for a scavenger hunt
platform. Our service will encourage players to run around their city, solving
clues, exploring new places, and doing challenges. As a minimum viable product,
we will collect periodic location data. Players can send geotagged photos
to our service, both of which we will use to track player
progression automatically.  For example, if a clue leads the player to the city library, they
can "prove" they solved the hint by going to the library and sending us a
selfie. Of course, there is no telling what the product people might have up
their sleeves for what sort of data we can use in the future, so building an
extensible system is a plus.

For every challenge the user completes, we'd like to reward them. That
reward might be just a point on the game leaderboard, or, for particularly
challenging tasks, it might be something more tangible. The product people
suggest that one day we'd like to partner with local businesses as
part of our monetization strategy. This is another potential point of
extensibility.

Let's illustrate all of this. Perhaps one of the places in our scavenger hunt is
a monument on the hill, as indicated by the yellow marker in @fig:poi. If the
player sends us a selfie taken anywhere within the purple dashed circle, we will
reward them.

![Point of interest](images/poi.png){#fig:poi}

Suppose our player is currently standing at the orange lollipop in
@fig:starting-loc, when she solves the clue "*this three-fold monument overlooks
the city.*" She sets off towards the monument, taking the path indicated in
@fig:movement.

![The player's position](images/starting-loc.png){#fig:starting-loc}

![Movement towards the point of interest](images/movement.png){#fig:movement}

Every lollipop in @fig:movement is a snapshot of our player's location at a
moment in time while she moves towards the goal. We will receive periodic
updates of her location corresponding to these points.

When the player gets to the point of interest, she takes a selfie with the
monument and sends it to us, as in @fig:snap. The camera icon also comes with
geolocation data, which we will use to verify her location, rather than
manually looking at the image she sends us. Upon receipt of a selfie within the
purple circle on the map, we give the player her reward, and she is free to
tackle another challenge in the scavenger hunt.

![Snap!](images/snap.png){#fig:snap}

We expect there to be hundreds, if not thousands of challenges like these in the
initial launch, and that everyone in the company will need to help in coming up
with challenges --- including the non-technical people. As such, we'd like to
build a platform that doesn't require bespoke engineering effort for each
challenge. It needs to be as simple as possible for our non-technical colleagues
to design scenarios. In short: we will need to build a domain-specific language
for constructing these challenges.


Exercise

: Attempt a quick design of this system before continuing. Sketch the core data
  types and functions necessary for a scavenger hunt system like described
  above. How would you encode a challenge like "take a selfie at the library,
  and receive one point afterward."


Exercise

: Try to represent a challenge that requires players to go to the beach and to
  the library --- but in either order --- and then receive a reward. Can your
  initial design represent this problem? If not, modify the core data types to
  support this functionality.


Exercise

: What implications does your design have, in terms of its eventual usability,
  performance, complexity, and extensibility?


As is too often the case when building real software systems, we will not all at
once introduce all of the sorts of challenges we'd like to construct. Instead, we'll
discover new ones as we go, and keep an eye on how much our design needs to
wriggle to support them: the less wriggling, the more adaptable our
design must be.

A great place to begin any algebra is by determining some of its *observations,*
that is, the answer to"what is the purpose of this thing we're building?" Recall
that our scavenger hunt game will be receiving location data and geotagged
photos from the user, and will use that to determine what rewards the player
should receive. This is very suggestive of an observation: a function that maps
from a `Challenge` and a list of inputs to all of the rewards the player should
receive for having performed those inputs:

```haskell
data Challenge
data Input
data Reward

getRewards :: Challenge -> [Input] -> [Reward]
```

Here, a `Challenge` describes the scavenger hunt we're building. An `Input` is
an event that comes from the user --- either their current location sent
automatically by the app or an explicitly uploaded selfie. The list of inputs
is sorted in chronological order.

Because the Algebra-Driven Design approach forces us to be denotational, we cannot use mutation of variables, and thus must resort to receiving the
player's entire history to determine what rewards we should give them.
Remember that these types exist only as scaffolding. We will delay putting
giving them any internal structure for as long as possible. Right now, we know as
little about this problem as we ever will, and so any decisions we make now are
almost certainly going to be wrong.

With an observation and some data types in place, we can try our first jab at
making a challenge. The most straightforward challenge we will call a "point of interest,"
which has an associated clue and a position on the map. The player will be
presented with the hint, and if they solve it, they can go to the point of
interest and send us a photo from there. If it's in the right place, we'll
reward them with some points.

Such a challenge has three parameters --- the clue, the place players are
supposed to go, and the number of rewarded points. On second thought, let's also
add how close they must be to the exact location. For small points of
interest --- perhaps a statue --- we'd like them to be very close, but bigger
points of interest that are buildings or parks don't require much accuracy.
The difference in these "tolerances" is shown in @fig:gediminas and
@fig:cathedral.

![This statue is a point of interest. The yellow marker indicates the right
spot, while the dashed purple line shows the location
tolerance.](images/gediminas.png){#fig:gediminas}

![A large cathedral. Because it is such a dominating structure, we allow more
tolerance around it.](images/cathedral.png){#fig:cathedral}

```haskell
data Clue
data Point
data Distance

pointOfInterest
    :: Clue
    -> Point
    -> Distance
    -> Reward
    -> Challenge
```

We add more scaffolding data types for each parameter. Probably `Distance` will
eventually end up being a floating-point number, but again, there is no need to
decide yet. Once we have the full design mapped out, we will be better
positioned to make implementation choices like these. For now, it does not harm, and
in fact, acts as helpful documentation.

Armed with a mental model of what `pointOfInterest` is supposed to do, we are
faced with the challenge of formalizing those semantics. We'd like to say "if
the challenge is a `pointOfInterest`, and the user sends us a photo taken in the
proximity circle, then they should receive the associated reward." Easy enough,
but we will first need a few helper functions:

```haskell
photo :: Point -> Photo -> Input
within :: Point -> Point -> Distance -> Bool
```

The `photo` function allows us to construct a geotagged photo `Input`, and
`within` determines if two points are within `Distance` of one another.

```{.haskell law="point of interest"}
∀ (c :: Clue) (poi :: Point) (d :: Distance)
      (r :: Reward) (p :: Point) (pic :: Photo)
      (is :: [Input]).
  within poi p d =>
    getRewards
        (pointOfInterest c poi d r)
        (photo p pic : is)
      = [r]
```

What a mouthful! The clause `within poi p d` here states that this equation
holds only while points `poi` and `p` are within `d` distance of one another. If
they are, if the head of our list of inputs is a `photo`, then we should reward
the player with `r`. Because we don't want players to redeem the same challenge
reward multiple times, we stop processing the remaining inputs `is`.

Our `law:point of interest` law governs only what happens if the user takes a
photo in the right place. What if they are outside of the location tolerance? We
will need semantics for that case as well. We decide that there is no penalty
for uploading a selfie in the wrong place; after all, the player might figure
out the clue later, and should still be able to go collect their reward. This
implies the semantics we want are just to ignore any `photo` taken outside of
our tolerance. @Fig:bad-photo demonstrates this scenario.

![Our player mistakes the clue for a different building, and she uploads a
photo. When it doesn't work, she realizes her mistake and moves to the
cathedral.](images/bad-photo.png){#fig:bad-photo}

```{.haskell law="outside point of interest"}
∀ (c :: Clue) (poi :: Point) (d :: Distance)
      (r :: Reward) (p :: Point) (pic :: Photo)
      (is :: [Input]).
  not (within poi p d) =>
    getRewards
        (pointOfInterest c poi d r)
        (photo p pic : is)
      = getRewards
            (pointOfInterest c poi d r)
            is
```

Law `law:outside point of interest` states that the `getRewards` of a
`pointOfInterest` ignores any `photo` inputs that are not within the location
tolerance. It does this by dropping the `photo p pic` input from the input list and trying again with the remainder. In essence, `law:outside point of
interest` discards incorrect photos until `law:point of interest` finds a good
one.

But this is not all of the cases we need to consider to describe the semantics
of `pointOfInterest`! We also need to consider what happens if we receive a
location update from the player --- or any other *non-selfie* input, for that
matter. In case of any other input, the user hasn't sent us a photo and
shouldn't receive their reward. The logic for this should be equivalent
to `law:outside point of interest`; we must discard the unmatching input.

```{.haskell law="unmatching point of interest"}
∀ (c :: Clue) (poi :: Point) (d :: Distance)
      (r :: Reward) (i :: Input) (is :: [Input]).
  not (isPhoto i) =>
    getRewards
        (pointOfInterest c poi d r)
        (i : is)
      = getRewards
            (pointOfInterest c poi d r)
            is
```

Here we introduce a trivial observation `isPhoto :: Input -> Bool` which, as you
might guess, returns `True` if the input was constructed via `photo`. Otherwise,
`law:unmatching point of interest` is entirely equivalent to `law:outside
point of interest`.

Each of `law:point of interest`, `law:outside point of interest` and
`law:unmatching point of interest` is quite involved, not at all like the simple
laws we saw while working through the tiles algebra in @sec:tiles. You might be
wondering if this indicates the problem is *fundamentally more
complicated* than our last. It's possible, but complicated laws, more often than
not, suggest that our building blocks are too big. *Involved equations are a
classic symptom of designing the wrong algebra.*

Let's look back at our `pointOfInterest` constructor. It takes four parameters
and does three orthogonal things: informing the user (the `Clue`), describing an
area (the `Point` and `Distance`), and giving a `Reward`. This flies in the face
of our parsimony rule --- a constructor should do one thing and do it well.
Some careful analysis of the laws shows that all three completely ignore the
`Clue`, and that two of them disregard the `Reward` as well.  Whatever
`pointOfInterest` *is*, it doesn't seem to be about clues or rewards!

In cases like these, the solution is usually to start pulling arguments out into
simpler constructors. The `Clue` seems like a good candidate to be extracted
since it's completely unused in the semantics of `pointOfInterest` concerning our `getRewards` observation. We will pull it out into the `clue`
constructor:

```haskell
clue :: Clue -> Challenge -> Challenge
```

Our understanding is that `clue` will associate a `Clue` with a `Challenge`, but
we will delay thinking further about it. With a new constructor in play,
we should give it semantics relative to `getRewards`. And the semantics are
easy; whatever this `clue` thing is, it doesn't interact with `getRewards` at
all.

```{.haskell law="getRewards/clue"}
∀ (k :: Clue) (c :: Challenge).
  getRewards (clue k c) = getRewards c
```

Equations like `law:getRewards/clue` are the level of simplicity we're aiming
for, and the fact that we've found it suggests we are on the right track.

We can give the `Reward` parameter of `pointOfInterest` the same treatment of
pulling it into its own constructor. Rather than associating a `Reward` with an
existing `Challenge` however, experience suggests that this should be a terminal
constructor --- one which builds a `Challenge` ex nihilo. This choice is made
via intuition more than anything else, but I will try to motivate it by saying
that the `reward` is the last thing that happens in a challenge. Denotational
semantics allow us to explore trees only in a top-down fashion, meaning the leaf
is the thing we get to last. We find this temporal correspondence appealing.

```haskell
reward :: Reward -> Challenge
```

The `getRewards` semantics of `reward` are also straightforward; we simply
give back the `Reward`.

```{.haskell law="getRewards/reward"}
∀ (r :: Reward) (is :: [Input]).
  getRewards (reward r) is = [r]
```

To reiterate, the utter simplicity of `law:getRewards/clue` and
`law:getRewards/reward` is incredibly suggestive that we've carved our algebra
at its joints.


Exercise

:  Reduce `getRewards (clue c (reward r)) is` to its simplest form via algebraic
   manipulation.


What's left in `pointOfInterest`? Its `Point` and `Distance` parameters. Let's
try pulling those two parameters into a new constructor:

```haskell
photoWithin
    :: Point
    -> Distance
    -> Challenge
    -> Challenge
```

The idea here is that `photoWithin` waits until a photo has been taken within
the tolerance of the `Point`, before switching to its `reward` challenge. Again
we need to give semantics for it. If a `photo` is taken within tolerance of the
point of interest, we will give the rewards of our `Challenge` argument. But we
can't provide the rewards directly anymore, we must call `getRewards` on our
`Challenge` argument, which requires a list of inputs! We make the arbitrary
(though reasonable) decision that the `photo` input is "consumed" to
get past a `photoWithin`. That is to say, you can't use a single `photo` for
multiple challenges!

```{.haskell law="getRewards/photoWithin"}
∀ (poi :: Point) (p :: Point) (pic :: Photo)
      (d :: Distance) (c :: Challenge) (is :: [Input]).
  within poi p d =>
    getRewards (photoWithin poi d c) (photo p pic : is)
      = getRewards c is
```

While `law:getRewards/photoWithin` certainly has fewer binders than `law:point
of interest`, the presence of that `within` predicate means any set of complete
semantics is going to require laws analogous to `law:outside point of interest`
and `law:unmatching point of interest`. There's simply no way around it.
Unfortunately, it's unclear how to better simplify the semantics of `photoWithin`
further, so we will suffer it for the time being, and content ourselves with a
law relating our new constructors to `pointOfInterest`:

```{.haskell law="pointOfInterest"}
∀ (c :: Clue) (p :: Point) (d :: Distance) (r :: Reward).
  pointOfInterest c p d r
    = clue c (photoWithin p d (reward r))
```

Getting stuck is a good time to look at another example and see if it helps to
clarify things. After a quick meeting with our product manager, we learn that
the next sort of challenge we'd like is to submit a photo from a very high
altitude. Our PM explains that we'd like a clue that sends players to the
rooftop of whatever skyscraper they can get on top of. Following in the steps of
`photoWithin`, we sketch this out:

```haskell
data Altitude

photoAbove
    :: Altitude
    -> Challenge
    -> Challenge
```

You can imagine the `getRewards` semantics for `photoAbove`; we'd need to
introduce an `aboveAltitude :: Point -> Altitude -> Bool` observation, which
checks if a location is above a certain `Altitude`. From there, we would simply
replace our `within poi p d` predicate with an `aboveAltitude p a`. But if we
encoded it so directly, this wouldn't buy us anything --- remember, we're trying
to move away from the `within` predicate!


### Input Filters

If we take a step back, we can see similarities between `photoWithin` and
`photoAbove`: they both scan through the `Input` list until a satisfactory
`photo` is located --- though they have different definitions of "satisfactory."
The key insight here is that we can make a new combinator that does the
*scanning* inputs part, while being parameterized on the stopping criterion.
Such a thing *could* be specialized over only `photo` inputs, but recall that we
also get location heartbeats from our player, and that our product managers have
explicitly asked us to stay flexible in the sorts of input we can accept in the
future.

The logical endpoint of this is to introduce a new type corresponding to
interesting sorts of inputs. Let's call it an `InputFilter`, and it comes with
an observation that returns `True` if and only if an input matches the filter:

```haskell
data InputFilter

matches :: InputFilter -> Input -> Bool
```

With this machinery in place, we can write `InputFilter` analogues of
`photoWithin` and `photoAbove` --- in fact, let's just reuse those names and
forget about their `Challenge` variants, since we weren't happy with those
in any case.

```haskell
photoWithin :: Point -> Distance -> InputFilter
photoAbove :: Altitude -> InputFilter
```


Exercise

:  Give semantics in terms of `matches` for both `photoWithin` and `photoAbove`.
   Remember that these are now functions over `InputFilter`, not over `Challenge`
   like before. Does the new type simplify the semantics?


We can now give a `Challenge` constructor which waits on a particular
`InputFilter` before transitioning into a different `Challenge`. Let's call it a
`gate` because it acts as a parking gate, preventing forward progress until
the correct condition is met.


```haskell
gate :: InputFilter -> Challenge -> Challenge
```

The `gate` constructor is subject to the following law in the case it matches:

```{.haskell law="getRewards/gate"}
∀ (f :: InputFilter) (c :: Challenge) (i :: Input)
      (is :: [Input]).
  matches f i =>
    getRewards (gate f c) (i : is)
      = getRewards c is
```

which describes the semantics of passing through the gate: if `i` matches the
filter, gives the same results as the sub-challenge `c` does, under the
remaining inputs. The semantics of being "gated" --- that is, of *not* passing
--- are given by another law:

```{.haskell law="getRewards/gate unmatched"}
∀ (f :: InputFilter) (c :: Challenge) (i :: Input)
      (is :: [Input]).
  not (matches f i) =>
    getRewards (gate f c) (i : is)
      = getRewards (gate f c) is
```

which simply ignores `i` if it doesn't match. Again, these new equations are
dramatically simpler than what we started with, and simultaneously express more
power: we can now add new sorts of challenges just by adding a new
`InputFilter`, and because it needs only to map inputs to booleans, this is
significantly less effort. We've found a more extensible formulation of the
design space just by playing around with it --- and amazingly, we haven't
written any code yet!

We need one last equation constraining `getRewards` and `gate` --- describing
what happens if there are no more inputs to be had. In this case, there are no
rewards either, since the player never made it past the gate.

```{.haskell law="getRewards/gate empty"}
∀ (f :: InputFilter) (c :: Challenge).
  getRewards (gate f c) [] = []
```

Finally, we can now re-express `pointOfInterest` in terms of `gate`:

```{.haskell law="pointOfInterest"}
∀ (c :: Clue) (p :: Point) (d :: Distance) (r :: Reward).
  pointOfInterest c p d r
    = clue c (gate (photoWithin p d) (reward r))
```

While we're in the neighborhood, let's flesh out the part of our algebra that
deals with input filters. We'd like to make a challenge requiring the player to take a photo above a certain altitude, *while within a point of
interest?* Because an `InputFilter` is morally just a function to booleans (this
is the *meaning* of having the `matches` observation,) we should expect all of
the usual boolean values and operations to lift over input filters. Namely:

```haskell
always :: InputFilter
never  :: InputFilter

andF :: InputFilter -> InputFilter -> InputFilter
orF  :: InputFilter -> InputFilter -> InputFilter
notF :: InputFilter -> InputFilter
```

The laws are exactly what you'd expect, so we can "cheat" and give homomorphisms
into booleans --- rather than showing that these things form a boolean algebra
directly. This is a good trick when embedding well-known algebras into your own.

```{.haskell law="matches/always"}
∀ (i :: Input).
  matches always i = True
```

```{.haskell law="matches/never"}
∀ (i :: Input).
  matches never i = False
```

```{.haskell law="matches/andF"}
∀ (f1 :: InputFilter) (f2 :: InputFilter) (i :: Input).
  matches (andF f1 f2) i = matches f1 i && matches f2 i
```

```{.haskell law="matches/orF"}
∀ (f1 :: InputFilter) (f2 :: InputFilter) (i :: Input).
  matches (orF f1 f2) i = matches f1 i || matches f2 i
```

```{.haskell law="matches/notF"}
∀ (f :: InputFilter) (i :: Input).
  matches (notF f) i = not (matches f i)
```


### Simultaneous Challenges

So far, we have always used a `reward` as the child to `gate`, but this is not
mandated. As a fascinating byproduct of having carved up
`pointOfInterest` into smaller constructors, we can now express strictly more
terms.


Exercise

:  Consider the term `gate (photoWithin p1 d1) (gate (photoWithin p2 d2) (reward r))`.
   What is your intuitive understanding of this expression? Do our stated
   semantics agree with you? Hint: try evaluating `getRewards` of the above,
   using two different photo inputs.


Like the previous exercise shows, nothing prevents us from putting one `gate`
inside another. This is a syntactic consequence of splitting our big
combinators into little ones, but as the exercise shows, the semantics of such
an operation are automatically preserved! By decomposing our problem in this
way --- without adding any additional features --- we can now express challenges
that we never anticipated. As @fig:two-pois shows, it's now possible to construct
challenges that involve visiting several points of interest in a prescribed
order.

![A challenge with two points of interest, which must be "snapped" in the
correct order.](images/two-pois.png){#fig:two-pois}

We can use this newfound behavior to create entirely new sorts of challenges,
where players must walk past certain checkpoints in the right order. We could
use this functionality to encourage players to walk down particularly beautiful
alleyways, or to see parts of the city from an unconventional point of view.
Let's add a new `InputFilter` that only uses our passive location data in order
to see where our players are walking:

```haskell
locWithin :: Point -> Distance -> InputFilter
```


Exercise

:  Use `locWithin` to encode a challenge that requires our player to walk around
   the block twice, clockwise, as in @fig:trace-block. Assume you have `p1, p2, p3 :: Point`
   and `d1, d2, d3 :: Distance` corresponding to each corner's locations and tolerances.


![A challenge to walk around the block twice.](images/trace-block.png){#fig:trace-block}

It's very encouraging how our vocabulary of possible challenges widens like
this, by merely adding tiny new combinators.

This notion of sequentiality raises an interesting question; can we encode a
challenge in which players can visit several points of interest in *any* order?
The scenario illustrated in @fig:many-poi asks players to
tour every green space in the city center. The number of different ways of
visiting these parks grows super-exponentially --- so we cannot realistically
make a "variation" of the challenge for every possible path the player takes. We
need a new building block.

![A challenge to snap every park in the city. We cannot reasonably expect the
player to visit the parks in any specific
order.](images/many-poi.png){#fig:many-poi}

Of course, we could always hard-code this functionality in with a bespoke
constructor --- one reminiscent of `pointOfInterest`, but which takes a *list*
of points of interest --- but this feels heavy-handed. There are certainly more
elegant and more reusable building blocks we can carve out, which engender this
functionality, and a host of features we haven't yet considered.

In essence, what we're asking for is for some sort of "super-`Challenge`:" one
which finishes only after all of its children do. The simplest formulation of
such a thing is `both`:

```haskell
both :: Challenge -> Challenge -> Challenge
```

The idea here is that we can build a `Challenge` which involves visiting two
parks in either order, simply by joining two individual challenges --- each
requiring a visit to a single park. If we'd like to require visiting more parks,
as in @fig:many-poi, we can nest calls to `both`, building a sort of binary
tree. For example, we might model @fig:many-poi as a balanced tree:

```haskell
both (both (both park1 park2) (both park3 park4))
     (both (both park5 park6) park7)
```

where each `parkN :: Challenge` is a point of interest challenge.

As usual, we will need to give laws for `both`. Rather than diving directly into
its semantics, we will note that our intuition of needing complete both
sub-challenges is highly indicative that the order of the arguments doesn't
matter. That is, `both` should be commutative:


```{.haskell law="both:commutative"}
∀ (c1 :: Challenge) (c2 :: Challenge).
  both c1 c2 = both c2 c1
```

and we should also expect the parentheses in a large nesting of `both` terms to
not matter:

```{.haskell law="both:associative"}
∀ (c1 :: Challenge) (c2 :: Challenge) (c3 :: Challenge).
  both c1 (both c2 c3) = both (both c1 c2) c3
```

Additionally, we will add a law stating that `both` is *idempotent.* This is a
difficult thing to motivate other than it "feels right." My argument is that the
challenges happening inside a `both` term are co-occurring; if you
were asked to do the same thing twice *simultaneously*, surely that is
equivalent to doing it only once.

```{.haskell law="both:idempotent"}
∀ (c :: Challenge).
  both c c = c
```

Exercise

:  Prove that `both (both c1 c2) (both c3 c4) = both c1 (both c2 (both c3 c4))`.
   How might an implementation use this fact?


Exercise

:  There is only one "reasonable" semantics for threading inputs through `both`
   with respect to `getRewards`. What is it?


```{.haskell law="getRewards/both"}
∀ (c1 :: Challenge) (c2 :: Challenge) (is :: [Input]).
  getRewards (both c1 c2) is
    = getRewards c1 is <> getRewards c2 is
```

There is a contradiction implied by `law:getRewards/both` and `both:commutative`
that I didn't notice when first writing this section. Rather than correcting the
prose's error, I've left it in and discuss how I noticed it and how to fix it in @sec:simplification.

In English, `law:getRewards/both` says something like "the rewards of both are
both the rewards." It looks good, but things are not yet all well. The semantics
of `both` give us rewards as we visit each point of interest, rather than
waiting for us to see them all and only then receiving our just rewards. There is a
choice to be made here, between two very reasonable-sounding challenges:

1. Visit all seven of the parks, and then get a big reward.
2. Visit three different countries, getting a reward for each, and a big reward
    for completing all of them.

In the first case, the necessary components of the `Challenge` are easy enough
that the player probably won't be upset if she doesn't get the big reward. On
the other hand, imagine a player only managing to get to two out of three
*countries* within the time limit, and was given nothing to show for it! It
would be horrible public relations indeed!

From this analysis, we realize we are missing two different capabilities: the
ability to create a `Challenge` that has no reward at the end (useful for making
up more significant challenges that *do* come with prizes), and the ability to attach a
reward to having completed both branches of a `both`. Let's tackle the first
one to start.

Recall that we use the `reward` constructor to end a challenge, resulting in the
player receiving the reward. Because `reward` is the only terminal constructor
for `Challenge`, we have no other way of syntactically ending a `Challenge`
expression. The solution is simple; we just add a terminal constructor
corresponding to no rewards. Let's call it `empty`:

```haskell
empty :: Challenge
```

with the obvious semantics:

```{.haskell law="getRewards/both"}
∀ (is :: [Input]).
  getRewards empty is = []
```

Less obvious is that `empty` acts as an identity element for `both`, which is to
say, that you can always connect an `empty` to some other `Challenge` via `both`
without changing the result. More formally:

```{.haskell law="both:identity"}
∀ (c :: Challenge).
  both empty c = c = both c empty
```

The laws `law:both:associative`  `law:both:identity` are sufficient to show that
`both` forms a monoid (@sec:monoid). Being a monoid is a boon to both our
implementation and our users; it means we can relieve users of needing to build
*balanced* trees of `both` terms. Instead, we can balance it (or do any other
associative optimization)  for them --- without changing the meaning of the
expression. As you can see, it is often valuable to consider how every new
constructor relates to the existing ones.


### Challenge Completion

In the last section, our analysis suggested that we were missing a means of
"joining" two forked challenges --- that is to say, requiring further actions
from the player after both branches of a `both` term are completed. Recall, this
is so that we can give a big reward after the competition of several sub-tasks. We
can encode such a thing with an explicit sequencing constructor for challenges:

```haskell
andThen :: Challenge -> Challenge -> Challenge
```

Our understanding of `andThen` is that the second argument should happen after
the first is "finished." We don't yet have a robust notion of `Challenge`
completion which will be necessary for giving the full semantics of this
operation, but we do already have some implicit sequencing provided by the `gate`
combinator. Because `andThen` and `gate` both deal with sequencing in some way,
we should give a law stating the interaction between these two constructors.
It's not hard to convince ourselves that `andThen` should distribute through
`gate` --- in either case, the first thing we will do is wait for the
`InputFilter` to match, before proceeding to both actions.

```{.haskell law="andThen/gate"}
∀ (f :: InputFilter) (c1 :: Challenge) (c2 :: Challenge).
  andThen (gate f c1) c2 = gate f (andThen c1 c2)
```

Although it seems like we might like to do the same transformation for `clue`,
this is a non-starter. Why? It changes the scope of a challenge! If we were to
rewrite `andThen (clue k c1) c2` as `clue k (andThen c1 c2)` we'd need to wait
until `c2` is all the way finished before completing the clue --- rather than
immediately after completion of `c1`.

Turning our attention to the algebraic structure of `andThen`: it should be
associative; different parenthesizing strategies can't reorder the things we're
asking of our players.

```{.haskell law="andThen:associative"}
∀ (c1 :: Challenge) (c2 :: Challenge) (c3 :: Challenge).
  andThen c1 (andThen c2 c3) = andThen (andThen c1 c2) c3
```

And remember that the `empty` constructor is a challenge in which nothing
happens, so we can expect it to be an identity element here as well.

```{.haskell law="andThen:identity"}
∀ (c :: Challenge).
  andThen empty c = c = andThen c empty
```

It's remarkable how many monoids turn up when you're looking for them.

We are now faced with determining the semantics of what it means
for a `Challenge` to be completed. Remember: *semantics must always be relative
to an observation.* Our only observation for challenges thus far is
`getRewards`, which doesn't seem like a likely candidate for determining if a
challenge is completed --- after all, the entire purpose of this machinery we're
building is for challenges to be able to give rewards "part way through." The
conclusion: we require a new observation.

Completing a `Challenge`, like the rewards it grants, depends on
the inputs we receive from the player. And the state of being completed appears
to be a boolean.  This might suggest that our observation should be:

```haskell
completes :: Challenge -> [Input] -> Bool
```

but, speaking with future-knowledge here, such an approach doesn't work. The
issue is that this formulation of `completes` is insufficiently compositional;
yes, it tells us whether or not the `Challenge` was completed, but it doesn't
tell us how much input was required to do so. Everything would work out fine
until we got to the semantics for `andThen`, which is unable to describe how
many inputs it consumed in completing the first branch.


Exercise

:  Give semantics for `andThen` in terms of `completes :: Challenge -> [Input] -> Bool`.
   Show that these semantics necessarily contradict `law:andThen/gate`.


While there is undoubtedly an art to choosing sound observations, this is an excellent
example of what goes wrong when you choose a bad one --- not much! If your decisions were based on unfounded ideas at any point, you'll run into an
inconsistency sooner than later. No harm, no foul; contrast the contradicting
pieces and keep whichever has the stronger foundations. It's helpful to remind
yourself (and your manager) that this is not wasted time; these are
inconsistencies you'd have run into anyway if you'd dived directly into the
implementation --- but at that point, it would be significantly more costly to
fix.

The problem with our first attempt at `completes` is that it suffers from
boolean blindness. To be completed, a `Challenge` will consume
zero-or-more inputs, but we lose that information when returning only a boolean.
Instead, we will give a subtly different type for `completes` --- one which
returns its unconsumed inputs if it completed, or `Nothing` if it hasn't
finished yet. We can't only use an empty list of outgoing inputs to signal
non-completion --- perhaps a `Challenge` was completed on the last input.

```haskell
completes :: Challenge -> [Input] -> Maybe [Input]
```

Our new type for `completes` now tracks what input it hasn't consumed, so it can
be used to perform sequencing, as we'll see when we look at the semantics for
`andThen` under `completes`.

Our intuition is that both `reward` and an `empty` challenges are always
completed, regardless of their inputs.

```{.haskell law="completes/empty"}
∀ (is :: [Input]).
  completes empty is = Just is
```

```{.haskell law="completes/reward"}
∀ (r :: Reward) (is :: [Input]).
  completes (reward r) is = Just is
```

And indeed, a `both` challenge is completed if and only if both of its children
are. But here we must be careful about what input isn't consumed; we would like
to return the shorter of the two unconsumed streams, which is to say, the branch
which used *more* of its input. This ensures that input consumed by either
of our branches doesn't "leak out" and accidentally get counted twice.

```{.haskell law="completes/both"}
∀ (c1 :: Challenge) (c2 :: Challenge) (is :: [Input]).
  completes (both c1 c2) is =
    shorterOf <$> completes c1 is <*> completes c2 is
```

where `shorterOf :: [a] -> [a] -> [a]` is a function which takes the shorter of
two lists.


Exercise

:  Give a law for the behavior of `shorterOf`.


Exercise

:  Does `shorterOf` form a monoid? If so, give it. If not, show which laws it
   doesn't satisfy.


As usual, we can ignore a `clue` term:

```{.haskell law="completes/clue"}
∀ (k :: Clue) (c :: Challenge) (is :: [Input]).
  completes (clue k c) is = completes c is
```

Next, we must tackle the `gate` constructor. Similar to the semantics for
`getRewards`, we can match on the incoming input stream, passing through if it
`matches`.

```{.haskell law="completes/gate"}
∀ (f :: InputFilter) (c :: Challenge) (i :: Input)
      (is :: [Input]).
  matches f i =>
    completes (gate f c) (i : is) =
      completes c is
```

There is an analogous law to `law:getRewards/gate unmatched`:

```{.haskell law="completes/gate unmatched"}
∀ (f :: InputFilter) (c :: Challenge) (i :: Input)
      (is :: [Input]).
  not (matches f i) =>
    completes (gate f c) (i : is) =
      completes (gate f c) is
```

And, at the end of the day, if we've completely run out of inputs, we can
conclusively say that we didn't complete the `gate`:

```{.haskell law="completes/gate empty"}
∀ (f :: InputFilter) (c :: Challenge).
  completes (gate f c) [] = Nothing
```

Let's step back for a moment and reiterate why we're doing all of this work.
We'd like to model challenges like @fig:many-poi2, which require
doing several things in parallel and then receiving a reward only after every sub-task. To facilitate our syntactic obligations
to even describe such a thing, we added the `andThen` constructor to
our algebra. We then worked through the semantics of `completes`, allowing
us to express when `andThen` should consider its first argument finished and
switch to its second. Now that we've got our `completes` observation, the answer
is clear:

![A challenge to snap every park in the city.](images/many-poi.png){#fig:many-poi2}

```{.haskell law="completes/andThen"}
∀ (c1 :: Challenge) (c2 :: Challenge)
    (is :: [Input]).
  completes (andThen c1 c2) is =
    completes c1 is >>= completes c2
```

The appearance of the bind operation `(>>=)` in `law:completes/andThen` is
highly appropriate; monadic binds are used to encode strict sequencing of
evaluation in lazy languages. This is a recurring theme when doing
Algebra-Driven Design; that our semantics transform from constructor to a
well-understood operation. Transformations like these are proof that our algebra
behaves like we say it should.

One last thing is to give a semantics of `andThen` for `getRewards`.

```{.haskell law="getRewards/andThen"}
∀ (c1 :: Challenge) (c2 :: Challenge)
    (is :: [Input]) (is' :: [Input]).
  completes c1 is == Just is' =>
    getRewards (andThen c1 c2) is =
      getRewards c1 is <> getRewards c2 is'
```

```{.haskell law="getRewards/andThen incomplete"}
∀ (c1 :: Challenge) (c2 :: Challenge)
    (is :: [Input]) .
  completes c1 is == Nothing =>
    getRewards (andThen c1 c2) is =
      getRewards c1 is
```


### Simplification {#sec:simplification}

Whenever possible, we should give distributive laws that allow for moving
constructors around relative to one another. After all, users will expect
it's acceptable to interchange any two operations that can be interchanged
conceptually. This is part of our promise never to break our users' conceptual
mental model that our software works exactly as it should.

One such distributive law is that if one branch in our `both` immediately gives
a reward, then we should be able to move that reward out front:

```{.haskell law="both/andThen/reward"}
∀ (r :: Reward) (c1 :: Challenge) (c2 :: Challenge).
  both (andThen (reward r) c1) c2 =
    andThen (reward r) (both c1 c2)
```

This law seems straightforward, but upon closer inspection it shows a
non-confluence in our laws. Consider the expression `both (reward r1) (reward
r2)`. We can give two different, lawful reductions for it:

```{.haskell .proof}
  both (reward r1) (reward r2)
= -- .via andThen:identity
  both (andThen (reward r1) empty) (reward r2)
= -- .via both/andThen/reward
  andThen (reward r1) (both empty (reward r2))
  = -- .via both:identity
andThen (reward r1) (reward r2)
```

We can contrast the above to:

```{.haskell .proof}
  both (reward r1) (reward r2)
= -- .via both:commutative
  both (reward r2) (reward r1)
= -- .via andThen:identity
  both (andThen (reward r2) empty) (reward r1)
= -- .via both/andThen/reward
  andThen (reward r2) (both empty (reward r1))
= -- .via both:identity
  andThen (reward r2) (reward r1)
```

The difference is subtle, but we can give two different lawful derivations of
which order we should receive rewards `r1` and `r2`. Does this matter? Not to us
as humans; we are indifferent to the order in which we receive our rewards, so
long as we get all of the ones we're supposed to.

As it happens, this is not an inconsistency caused by the addition of
`law:both/andThen/reward`; we can show it more directly using only
`law:getRewards/both`, `law:getRewards/reward`, and `law:both:commutative` ---
we just happened to miss it earlier:

```{.haskell .proof}
  getRewards (both (reward r1) (reward r2)) []
= -- .via getRewards/both
  getRewards (reward r1) [] <> getRewards (reward r2) []
= -- .via getRewards/reward
  [r1] <> getRewards (reward r2) []
= -- .via getRewards/reward
  [r1] <> [r2]
= -- .via definition of (<>)
  [r1, r2]
```

```{.haskell .proof}
  getRewards (both (reward r1) (reward r2)) []
= -- .via both:commutative
  getRewards (both (reward r2) (reward r1)) []
= -- .via getRewards/both
  getRewards (reward r2) [] <> getRewards (reward r1) []
= -- .via getRewards/reward
  [r2] <> getRewards (reward r1) []
= -- .via getRewards/reward
  [r2] <> [r1]
= -- .via definition of (<>)
  [r2, r1]
```

There is great tension here. Our algebra is inconsistent, which is a nice word
for *completely useless* --- a creative practitioner can show equality of any two
terms using this contradiction. The situation is similar to how any nonsense can
be "proven" in grade-school algebra after smuggling in a division by zero. At
its core, our tension exists between the `law:both:commutative` and the type of
`getRewards`. A list *simply isn't* commutative --- nor is it idempotent, for
that matter (looking at you `law:both:idempotent`).

We have a few choices of remedies here: we can abandon `law:both:commutative`
and `law:both:idempotent`; we can change the return type of `getRewards`; or we
can add a bunch of ad-hoc laws trying to shoe-horn commutativity and idempotency
into lists. When phrased in this way, the third option is the stupid
one; to experience some cognitive dissonance, contrast that feeling with the
industry-standard technique of enforcing these sorts of invariants in business
logic. **Invariants are a feature of the implementation, not of the design.**

The `law:both:commutative` law seems to be at the heart of receiving
rewards. Lists come with an inherent ordering --- contrary to this commutativity
we seek --- and we interpret this as *lists being the wrong data structure.* The
`law:both:idempotent` is on shakier ground, do we truly want to deny a
player from winning two separate rewards, each consisting of one point? Let's
strike idempotency from the desiderata of our new data structure.

What constraints are we left with? Our equations require any commutative
monoid[^why-monoid]. A set is probably the most characteristic commutative
monoid, but a set is idempotent --- something we explicitly don't want! Instead,
we settle on a multiset: a set containing a count of its contained duplicate
elements. This is an elegant solution to our problem as it keeps the laws we
care about and fixes the contradiction in one fell swoop.

[^why-monoid]: The structure must be a monoid, so we have a reasonable value to
  give back in case the player isn't eligible for any rewards.

```haskell
getRewards :: Challenge -> [Input] -> Multiset Reward
```

Our laws are unchanged for the most part; all that's required is replacing `[]`
with `mempty` and `[r]` with `singleton r`. As it happens, we didn't
use the fact that our reward output was a list --- even more evidence that it
was the wrong type in the first place!


### A Unified Observation

Thus far, we have been primarily giving operational semantics for our
constructors, that is to say, "how does it behave in the presence of these
observations?" Let's take some time and look at an alternative way of phrasing
our same semantics, this time in a more definitional manner.

Consider for example, evaluation of the term:

```{.haskell .proof}
  completes
    (both (gate always (reward r1)) (reward r2))
    [i]
= -- .via completes/both
  shorterOf <$> completes (gate always (reward r1)) [i]
            <*> completes (reward r2) [i]
= -- .via completes/reward
  shorterOf <$> completes (gate always (reward r1)) [i]
            <*> Just [i]
= -- .via completes/gate
  shorterOf <$> completes (reward r1) []
            <*> Just [i]
= -- .via completes/reward
  shorterOf <$> Just []
            <*> Just [i]
= -- .via definition of shorterOf
  Just []
```

This is quite a lot of effort on our part and feels a lot like *just running a
program.* Operational semantics are a lot closer to algorithms than they are to
data design. Instead, let's look at how we can simplify the derivation by doing
more algebra work "ahead of time:"

```{.haskell .proof}
  both
    (gate always (reward r1))
    (reward r2)
= -- .via andThen:identity
  both
    (gate always (reward r1))
    (andThen (reward r2) empty)
= -- .via both/andThen/reward
  andThen
    (reward r2)
    (both
      (gate always (reward r1))
      empty)
= -- .via both:identity
  andThen
    (reward r2)
    (gate always (reward r1))
```

We managed to eliminate the `both` term algebraically, without ever
needing to know we're in the context of `completes`. We're unable to
eliminate the `gate always` term, since we don't have access to any inputs, but
it seems like it *should* be simpler to evaluate challenges than it has been
thus far. This is very suggestive that our observations are overfitted to define our semantics.

Take a look back at all of our laws for `getRewards` and `completes`; neither
one of them requires the inputs to be a list --- we simply need to pull elements off its head. Perhaps this, too, is overfitted?

Instead, all of our laws only ever look at one input at a time. We've been
duplicating the "traversal" logic to dispatch inputs throughout the term --- one for each of our observations. Surely there's
a way of abstracting over this logic?

To solve this problem, we need to find a *unified observation* for our algebra: an
observation from which *all other* observations can be implemented. This
observation unifies `getRewards` and `completes` into one operation, meaning we
can simply give our implementation once. And as we'll see, finding the right
unified observation often aggressively simplifies the algebra.

We will exploit the fact that we only ever need to look at one input at a time,
and because `reward r` can always be rewritten as `andThen (reward r) empty`,
that `empty` is the only truly "complete" challenge.

```haskell
step :: Maybe Input -> Challenge -> (Multiset Reward, Challenge)
```

Life gets much more comfortable when we phrase our semantics in terms of `step`
directly.[^niggle] For example, rather than the ad-hoc logic we found in
`law:completes/both`, the law for `law:step/both` turns out to be a delightful
homomorphism:

[^niggle]: The `Maybe Input` argument is an annoying niggle in this framing, and
  exists to allow us to "force" a final state, as described in a minute.

```{.haskell law="step/both"}
∀ (i :: Maybe Input) (c1 :: Challenge) (c2 :: Challenge).
  step i (both c1 c2) = both <$> step i c1 <*> step i c2
```

The behavior of `(<*>)` () is to monoidally append the `Multiset Reward` resulting
from each action. That is to say, the result of `both <$> step i c1 <*> step i
c2` will be of type `(Multiset Reward, Challenge)`, where the first element in
the tuple contains all of the rewards that were accumulated during the two
steps taken. If you're unfamiliar with applicative functors, @sec:applicative
will help clarify what's going on here.

Let's give the remainder of our constructors' semantics in terms of `step`, and
then look at what it has bought us in simplicity.

```{.haskell law="step/empty"}
∀ (i :: Maybe Input).
  step i empty = pure empty
```

```{.haskell law="step/reward"}
∀ (i :: Maybe Input) (r :: Reward).
  step i (reward r) = (singleton r, empty)
```

```{.haskell law="step/gate"}
∀ (i :: Input) (f :: InputFilter) (c :: Challenge).
  matches f i =>
    step (Just i) (gate f c) = step Nothing c
```

In `law:step/gate` we see the need for the `Maybe Input` parameter on `step`
(rather than using an `Input` directly.) Upon passing a gate, you should be
conferred the resulting rewards immediately, rather than being required to wait
for the next `step` to occur.

```{.haskell law="step/gate unmatched"}
∀ (i :: Input) (f :: InputFilter) (c :: Challenge).
  not (matches f i) =>
    step (Just i) (gate f c) = pure (gate f c)
```

```{.haskell law="step/gate nothing"}
∀ (f :: InputFilter) (c :: Challenge).
  step Nothing (gate f c) = pure (gate f c)
```

Each of these laws describes how a `Challenge` should handle or dispatch an
incoming event. There's nothing to do for `empty`, so we just give back an
`empty`. For `reward`, we emit the reward and produce an `empty` term. In
the case of a `gate`, we give back the gated `Challenge` on an input match, or
give back the gate itself if the filter fails to match.

But of particular interest is the upcoming `law:step/andThen` laws. Rather than
dealing with all of that `completes` nonsense, we simply dispatch the `Input` to
the first sub-challenge. If that results in the first argument stepping to
`empty`, we can then step to the second argument:

```{.haskell law="step/andThen complete"}
∀ (i :: Maybe Input) (c1 :: Challenge) (c2 :: Challenge)
      (r :: Multiset Reward).
  step i c1 == (r, empty) =>
    step i (andThen c1 c2) =
      join (r, step Nothing c2)
```

However, if `c1` steps to any other value, we would like to rebuild the
`andThen` term with `c1` being stepped:

```{.haskell law="step/andThen incomplete"}
∀ (i :: Maybe Input) (c1 :: Challenge) (c2 :: Challenge).
  step i c1 == (_, c1') && not (isEmpty c1') =>
    step i (andThen c1 c2) =
      andThen <$> step i c1 <*> pure c2
```

By phrasing our semantics in terms of `step`, rather than the more specific
`getRewards` and `completes` observations, we can rely on our other laws
to do most of the "computation" work. This simplifies our semantics and, in doing so, makes it easier for us to reason about our system. Let's bundle
everything up into a neat package:

```{.haskell law="pumpChallenge"}
∀ (c :: Challenge).
  pumpChallenge c
    = foldM (flip step) c   -- ! 3
    . (Nothing :)  -- ! 2
    . fmap Just  -- ! 1
```

Reading bottom to top, at [1](Ann) `pumpChallenge` turns its inputs into `Maybe`
inputs. At [2](Ann), it prepends a `Nothing` value --- ensuring that the challenge
gets stepped at least once, even if there is no input. Finally, at [3](Ann), it
folds over the input stream, computing the `step` after every item.

Just in case you're afraid we've lost something by combining our observations
into `step`, we can get them back again by projecting out of the unified
observation --- in fact, this is the entire purpose. We can use a unified
observation to implement all of our other observations:

```{.haskell law="getRewards"}
∀ (c :: Challenge).
  getRewards c
    = fst . pumpChallenge c
```

```{.haskell law="completes"}
∀ (c :: Challenge).
  completes c =
    (== empty) . snd . pumpChallenge c
```


### Symmetry

Imagine we'd like to give a scavenger hunt challenge encouraging our players to
try a new cuisine; perhaps Vietnamese food isn't trendy in our city (yet!)
If there are two possible Vietnamese restaurants, how can we give rewards to our
players for going to either? It's unlikely any of players will be able to eat
two bowls of pho in a row --- regardless of their newfound appreciation for the
soup.

Our algebra already contains a `both` constructor, and where there's a `both`,
there is usually a symmetric `either` as well.  Arguments from symmetry are
fantastic inspiration for discovering unrequested functionality.  There is both
a psychological and a practical underpinning for this --- the evolving
requirements of the client are likely to be along similar lines as what they've
asked for, and implementing a symmetric idea is generally no more effort than
doing the work once.

What would be the meaning of such an `either` construction? Our intuition begins
by thinking of `both` as a sort of boolean AND, and that therefore `eitherC`
corresponds to boolean OR --- complete with short circuiting. We can begin by
giving a new constructor:

```haskell
eitherC :: Challenge -> Challenge -> Challenge
```

We can keep the associativity and commutativity laws, as we expect by analogy
with boolean OR:

```{.haskell law="eitherC:associative"}
∀ (c1 :: Challenge) (c2 :: Challenge) (c3 :: Challenge).
  eitherC c1 (eitherC c2 c3) = eitherC (eitherC c1 c2) c3
```

```{.haskell law="eitherC:commutative"}
∀ (c1 :: Challenge) (c2 :: Challenge).
  eitherC c1 c2 = eitherC c2 c1
```


Exercise

:  Does `eitherC` form a monoid? If so, what is its identity element?



Similarly, we'd like our `andThen/reward` to distribute:

```{.haskell law="eitherC/andThen/reward"}
∀ (r :: Reward) (c1 :: Challenge) (c2 :: Challenge).
  eitherC (andThen (reward r) c1) c2 =
    andThen (reward r) (eitherC c1 c2)
```

To facilitate short-circuiting, we can give a law that an `eitherC` of
an `empty` is itself `empty` --- though we need to be careful that its other
argument isn't a `reward`.

```{.haskell law="eitherC/empty"}
∀ (c :: Challenge).
  not (isReward c) =>
    eitherC empty c = empty
```

Exercise

:  Reduce `eitherC (reward r1) empty` to its simplest form.


We will also need to give `step` semantics for `eitherC`, although they are
analogous to those for `both`:

```{.haskell law="step/both"}
∀ (i :: Maybe Input) (c1 :: Challenge) (c2 :: Challenge).
  step i (eitherC c1 c2) =
    eitherC <$> step i c1 <*> step i c2
```

It's interesting to see what other sorts of things we can do with the
short-circuiting behavior of `eitherC`. By calculatedly reducing one of its
arguments to `empty`, we can "cancel" the other side --- and this functionality
means we can create challenges that come with a time limit. Let's posit a new
`Input` that corresponds to a real-world time, and an input filter for working
with them:

```haskell
data Time

time      :: Time -> Input
afterTime :: Time -> InputFilter
```

By automatically sending `time` inputs at regular intervals, we can make
challenges that are time-aware. Perhaps we might like a `Challenge` that can only be
completed in the first hour of the scavenger hunt:

```haskell
eitherC (gate (afterTime oneHour) empty)
        (pointOfInterest ....)
```


Exercise

:  Encapsulate this timeout behavior in a new `timeout` constructor. Be sure to
   give it a type and sufficient laws to entirely specify its behavior.


Having done all of the exercises, the assiduous reader will be pleased to note
that `eitherC` does indeed form a monoid. Its identity element is the
`Challenge` that can never be completed --- that is, anything gated behind a
`never` filter. This `Challenge` is distinguished by way of being an identity
element, so let's add it as a constructor to our algebra:

```haskell
bottom :: Challenge
```

governed by the law:

```haskell
∀ (c :: Challenge).
  bottom = gate never c
```



### Clues

With most of our algebra wrapped up, we are left only with the pesky
issue of reporting clues. We've been putting off this observation for the
entirety of our discussion, having been unaware of precisely what semantics they
should have. But now we know a great deal about what this thing we're building
*is* --- and have a better idea of what clues are and how they should behave
relative to the rest of the constructors.

Clues correspond to metadata about the challenge tree, to be consumed directly
by the user. We can use them to decorate sub-trees of a `Challenge`, but nothing
forces us to; we can have secret challenges in our game that are never told to
the player, encouraging extra exploration (although probably you should be
informed if you get an unadvertised reward!)

Let's posit some constructors to build a `Clue`. The most obvious is one which
is made from a string intended for the user.

```haskell
hint :: String -> Clue
```

Because `clue` (the constructor for `Challenge` --- not the data type here)
terms can contain other clues, we need to ask ourselves what behavior we'd
expect in this case. Should we be able to nest clues?  It seems that the
answer should be "yes" --- returning to our example of visiting three countries
to get a big prize, it would be pleasing both to show the "visit three countries"
clue, and show sub-clues corresponding to which countries are eligible. As well,
if I were a player, I'd like some feedback showing that my expensive trip
counted!

To facilitate this nesting of clues, perhaps we could give them a
nesting semigroup operation:

```haskell
sub :: Clue -> Clue -> Clue
```

with the usual associative law:

```{.haskell law="sub:associative"}
∀ (k1 :: Clue) (k2 :: Clue) (k3 :: Clue).
  sub k1 (sub k2 k3) = sub (sub k1 k2) k3
```

We will also expose an observation to "unroll" a clue into a list, where the
head of the list is the parent-most clue, and deeper sub-clues occur further
down the list:

```haskell
toList :: Clue -> [String]
```

with the semantics:

```{.haskell law="toList/hint"}
∀ (s :: String).
  toList (hint s) = [s]
```

```{.haskell law="toList/sub"}
∀ (k1 :: Clue) (k2 :: Clue).
  toList (sub k1 k2) = toList k1 <> toList k2
```

This homomorphism with lists suggests that `Clue` should form a monoid ---
lists, of course, being *the* canonical monoid. Perhaps we can posit the
existence of an identity element for `Clue`:

```haskell
noClue :: Clue
```

with the usual identity laws:

```{.haskell law="sub/mempty"}
∀ (k :: Clue).
  sub k noClue = k = sub noClue k
```

Before going too much further, we should slow down and think through this clue design's implications. It seems reasonable to give the following law:

```{.haskell law="clue/noClue"}
∀ (c :: Challenge).
  clue noClue c = c
```

Laws are most elegant when they preserve meaningful structure, so we should
consider how `sub` distributes over `clue`. Equally reasonably, we can give the
law:

```{.haskell law="clue/sub"}
∀ (k1 :: Clue) (k2 :: Clue).
  clue (sub k1 k2) c = clue k1 (clue k2 c)
```

The law `law:clue/sub` is really the only possible thing we could do for `clue`
and `sub` --- but do we have any better arguments for why it might be true?
Indeed we do; it might not look like it, but `law:clue/noClue` and
`law:clue/sub` form a monoid homomorphism, not to any monoid in the `Challenge`
algebra, but to the monoid over endomorphisms[^endomorphism]:

[^endomorphism]: Any function from a type to that same type. For example,
  `Int -> Int`, `String -> String` and `(String -> Bool) -> (String -> Bool)`
  are all endomorphisms, but `String -> Bool` itself isn't.

```{.haskell law="endo mempty"}
mempty = id
```

```{.haskell law="endo mappend"}
∀ (f1 :: a -> a) (f2 :: a -> a).
  f1 <> f2 = f1 . f2
```

With this new terminology, we can show the monoid homomorphism more directly:

```{.haskell .proof}
  clue mempty
= -- .via clue mempty
  clue noClue
= -- .via clue/noClue
  id
= -- .via endo mempty
  mempty
```

and

```{.haskell .proof}
  clue (k1 <> k2)
= -- .via clue mappend
  clue (sub k1 k2)
= -- .via clue/sub
  clue k1 . clue k2
= -- .via endo mappend
  clue k1 <> clue k2
```

It's nice to have a firm argument backing up our intuition.

Because clues are feedback meant for the end-user, once a player has seen a
clue, it should never be able to go "off the radar." We'd like to inform a
player if they've successfully completed the challenge behind a clue, and also
inform them if a clue is no longer completable (for example, if it was on the
short-circuited side of an `eitherC`.) From this analysis, clues can be in one
of four states: undiscovered, seen, completed, and failed.

We need an observation to get the `clue` data out of our algebra,
and that observation should reflect these states. First, the states themselves:

```haskell
data ClueState

seen      :: ClueState
completed :: ClueState
failed    :: ClueState
```

The "undiscovered" state is implicit, in that it exists somewhere in the
`Challenge` tree that we haven't yet gotten to.

To collect the clues and their states, we can give an observation that
takes a `Challenge` and produces a `MonoidalMap`.[^monoidal-map]

[^monoidal-map]: In Haskell, the standard `Map` data structure comes equipped
  with peculiar choice of semigroup instance --- one which is seldom what
  you want. Instead, we use `MonoidalMap` which is just a regular `Map`, except
  with a sensible semigroup instance that combines the values of colliding keys
  using a semigroup.

```haskell
getClues
    :: Challenge
    -> [Input]
    -> MonoidalMap Clue ClueState
```

But where is the `ClueState` monoid for us to map monoidally over?

The issue is this: we have no guarantees that any particular clue is unique.
Consider the expression `both (clue k c1) (clue k c2)` which assigns the same
clue name to two different challenges. In this case, `c1` and `c2` will be in
different clue states, and we need a strategy for which one to report back under
the name `k`. The solution is that because `seen` represents the least
information about a clue, we bias *against* returning `seen`. Similarly, in a
conflict between `failed` and `completed`, it seems cruel to deprive the player
of their `completed` result. Our preferences for clue states, in turn, gives rise
to a desirability ordering:

```haskell
seen < failed < completed
```

The `ClueState` monoid operation we're looking for is thus to take the maximum
of the two `ClueStates` under this ordering --- using `seen` as the identity
element. This ordering forms not only a monoid but also a *semilattice,*
reflecting our intuition that we must find a consistent state for
the clue, despite potentially conflicting inputs. Semilattices are described in
further detail in @sec:semilattice.

Like `getRewards`, we will implement `getClues` in terms of the `step`
unified observation, but doing so requires we change its type from:

```haskell
step :: Maybe Input -> Challenge -> (Multiset Reward, Challenge)
```

to

```haskell
step
    :: Clue  -- ! 1
    -> Maybe Input
    -> Challenge
    -> ( ( Multiset Reward
         , MonoidalMap Clue ClueState  -- ! 2
         )
       , Challenge
       )
```

where at [1](Ann) we now take a new `Clue` parameter corresponding to the
current "clue context." The context is necessary to properly nest
clues, as we have no other way of keeping track "where we are" in the
`Challenge` tree. The added `MonoidalMap` at [2](Ann) collects clues and their
state.

All of our previous laws for `step` need to be updated now in order to pass
their clue context parameter as an argument to their recursive calls. For
example, `law:step/both` should now look like this:

```{.haskell law="step/both"}
∀ (kctx :: Clue) (i :: Maybe Input) (c1 :: Challenge)
      (c2 :: Challenge).
  step kctx i (both c1 c2) =
    both <$> step kctx i c1 <*> step kctx i c2
```

All that's left is to give semantics for how `step` should interact with the
`ClueState` map. The easy case is `clue k empty`, which means the clue's subtree
has been completely stepped away --- which is to say, that it's been completed.
Such a term should also step to `empty`, but it should emit a `completed` clue
state in the process.

```{.haskell law="step/clue/empty"}
∀ (kctx :: Clue) (i :: Maybe Input) (k :: Clue).
  step kctx i (clue k empty) =
    tellClue (singleton (sub kctx k) completed) *> empty
```

Notice the call to `sub` here. We must prepend the current `Clue`
with the context before emitting it.

For any *other* `clue` term that we step, we realize it must not yet be
completed. Because `failed` clues appear only in the pruned branches of
`eitherC` (where they are presumably no longer stepped), the only remaining
option is to emit the clues as being `seen` and step their argument:

```{.haskell law="step/clue non-empty"}
∀ (kctx :: Clue) (i :: Maybe Input) (k :: Clue)
      (c :: Challenge).
  not (isEmpty c) =>
    step kctx i (clue k c) =
      tellClue (singleton (sub kctx k) seen) *>
        clue <$> pure k <*> step (sub kctx k) i c
```

There is a second call to `sub` here, in addition to what we saw in
`law:step/clue/empty`. Because we're entering a `clue` term, we update the clue
context, appending `k` to its previous value. Tracking the clue context in this
way ensures that we always know the exact nesting of any `clue` term we try to
step.

The final case is for clues that have been eliminated due to short-circuiting
`eitherC` challenges. The idea here is to find all `seen` clues in the pruned
branch and emit those as `failed` instead. Let's posit the existence of a helper
function:

```haskell
findClues
    :: Clue
    -> Challenge
    -> MonoidalMap Clue ClueState
```

which we will define in a moment, but the intuition behind is that it finds all
of the currently accessible clues in the given `Challenge`. Armed with
`findClues`, we can amend `law:step/eitherC`:

```{.haskell law="step/eitherC empty"}
∀ (kctx :: Clue) (i :: Maybe Input) (c1 :: Challenge)
      (c2 :: Challenge) (c2' :: Challenge) (z1 :: _)
      (z2 :: _).
  step kctx i c1 == (z1, empty) &&
  step kctx i c2 == (z2, c2') =>
    step kctx i (eitherC c1 c2) =
      fmap seenToFailed (findClues kctx c2') *>
        step kctx i c2 *>
          step kctx i c1
```

This law is rather involved, so let's take our time going through it. If the
result of stepping `c1` results in `empty`, then we should prune the second
branch. However, we can't do this immediately, because our semantics say that
the two branches should step simultaneously. Therefore we must step `c2`
into `c2'`, and then emit any `seen` clues present in it with `failed` clues ---
as suggested by the function `seenToFailed`. We then *actually* step `c2` to
make sure that any unlocked rewards are correctly given (and to
record any last-minute completed clues in that branch.) Then we must step `c1`,
to do the same, but we know that `c1` steps to `empty` and therefore
that the entire `eitherC` term does as well.

The corollary to `step/eitherC` is easier; in the case that neither `c1` nor
`c2` steps to `empty`, we simply rebuild the `eitherC` term with its newly
stepped children:

```{.haskell law="step/eitherC non-empty"}
∀ (kctx :: Clue) (i :: Maybe Input) (c1 :: Challenge)
      (c2 :: Challenge) (c2' :: Challenge) (z1 :: _)
      (z2 :: _).
  step kctx i c1 == (z1, c1') &&
  step kctx i c2 == (z2, c2') &&
  not (isEmpty c1') && not (isEmpty c2') =>
    step kctx i (eitherC c1 c2) =
      eitherC <$> step kctx i c1 <*> step kctx i c2
```

Marvelous. We need still to give a specification for `findClues`, and present it
here as as pseudo-code due to it doing exactly what you'd expect --- simply
traversing all of the visible terms of a `Challenge`:

```haskell
findClues
    :: Clue
    -> Challenge
    -> MonoidalMap Clue ClueState
findClues _    empty = mempty
findClues kctx (both c1 c2)
  = findClues kctx c1 <> findClues kctx c2
findClues kctx (eitherC c1 c2)
  = findClues kctx c1 <> findClues kctx c2
findClues _    (gate _ _) = mempty
findClues kctx (andThen c _)    = findClues kctx c
findClues kctx (reward _) = mempty
findClues kctx (clue k empty)
  = singleton (kctx <> [k]) completed
findClues kctx (clue k c)
  = singleton (kctx <> [k]) seen
    <> findClues (kctx <> [k]) c
```


### Generalization

Our algebra is now completed; it tackles all of our example scenarios using very
general machinery. The machinery is so general that we haven't used any
maps to help guide our intuition in quite some time. Now that our algebra has
sufficient capability, we can look at it as a whole, and see where we can dial
back some of its unnecessary constraints.

The best place to start is by looking at which types exist in our algebra as
placeholders but never made it into any equations. Throughout the design
process, we made up lots of types to help us reason through the specifics:
things like `Photo`, `Point`, `Distance`, `Altitude` and `Time`. While we have
*constructors* which use these types, the algebra itself --- that is, the laws
that govern its behavior --- is utterly agnostic to these types. While they
might be part of the specific application we're building, they have no place in
the reusable machinery we're building.

By looking at where these types are used, we find them appear only in the inputs
and input filters. But the inputs and filters are a self-contained piece of the
algebra; the only thing that our central `Challenge` machinery cares about is the
`matches` observation on inputs. This makes our `Input` type an excellent
candidate to be replaced with a type variable. The change is minor: we must
replace every `Input` in the algebra with `i`, and then parameterize `Challenge`
and `InputFilter` by `i`:

```haskell
data Challenge i
data InputFilter i

matches :: InputFilter i -> i -> Bool
gate    :: InputFilter i -> Challenge i -> Challenge i
-- etc.
```

This is a step in the right direction but isn't quite enough. Recall that input
filters correspond to predicates on inputs but also come with some shared
machinery in the form of `andF` and friends. As such, input filters are neither
completely the same nor completely apart from the inputs they match. One
possibility would be to provide an input filter constructor `(i -> Bool) ->
InputFilter i` which lifts a predicate into an input filter --- but such a
choice would prohibit some potential implementations, and as we will soon
see, buys us nothing for that price.

Instead, we can separate the reusable "boolean-like" aspects of the input filter
machinery from the input-specific ones (things like `photoWithin` and
`afterTime`). That is, we will introduce a new type parameter `f` corresponding
to the input-specific pieces; and allow the application engineer to customize
the filters as she pleases. To do so, we will need an ad-hoc
relationship between `f` and `i`, proving that the type `f` can match an `i`:

```haskell
class FilterFor f i | f -> i where
  filterMatches :: f -> i -> Bool
```

This change gives us a second type parameter `f` to sling around throughout our
code, in addition to `i`. But `f` and `i` are tightly coupled, so as a matter of
usability (but not of correctness, or generality), we can relate `i` to `f` more
directly:

```haskell
class HasFilter i where
  data CustomFilter i  -- ! 1
  filterMatches :: CustomFilter i -> i -> Bool
```

The line marked by [1](Ann) says that for any input `i`, there is a
corresponding type called `CustomFilter i` which, as you might expect, models
the custom filters on `i`. This means that the implementer of a set of input
filters gets to decide which sorts of inputs are matched by their filter --- and
that users of our library only ever need to thread around one type variable.

```haskell
data Challenge i
data InputFilter i

matches :: InputFilter i -> i -> Bool
gate    :: InputFilter i -> Challenge i -> Challenge i
-- etc.
```

To finish up this generalization we require a function that lifts a
`CustomFilter i` into an `InputFilter i`.

```haskell
custom :: HasFilter i => CustomFilter i -> InputFilter i
```

Input filter implementers can use `custom` to implement a more general `(i ->
Bool) -> InputFilter i` if such an operation isn't prohibitively expensive for
their implementation.

With the specific details of inputs abstracted away, the reusable parts of our
challenge algebra can be deployed in a broader set of circumstances; for example,
we can now use the same challenge design techniques to build online campaigns,
where there is no direct analogue of physical location. But we can generalize
further.

Of the remaining types, `Reward` is the most application-specific. But unlike
`Input` before it, rewards are explicitly moved around by the challenge algebra,
and are directly observable via `getRewards` and `step`. The insight here is
that while the `Reward` type itself is necessary for the algebra, its actual
contents are never inspected; we simply take a reward given by `reward` and
move it directly to the output --- as witnessed by `law:step/reward`:

```{.haskell law="step/reward"}
∀ (i :: Input) (r :: Reward).
  step i (reward r) = (singleton r, empty)
```

The generalization here is self-evident; we can replace `Reward` with any type
`r`, and empower application programmers to decide for themselves what sorts of
rewards are relevant in their problem domain.

Perhaps more interestingly, our choice of a `MultiSet` for observing the
final rewards was a byproduct of the specific choice we made --- if a reward is
a number of points, we'd like to allow for duplicates. But not everyone
will want duplicates; perhaps the idempotency of rewards is crucial to them.
Whatever the case, the more convincing argument is that nothing in our algebra
requires a `MultiSet`; we only ever use its monoidal interface. However, the
laws `law:both:commutative` and `law:eitherC:commutative` are both instrumental
in our algebra, and so whatever reward type we choose must also be commutative.

We can generalize our algebra accordingly. It will require a new type variable
corresponding to the type of rewards, and some new constraints on `step`:

```haskell
data Challenge i r

reward :: r -> Challenge i r

step
    :: ( IsFilter i
       , Monoid r
       , Commutative r
       )
    => Clue
    -> Maybe (Input i)
    -> Challenge i r
    -> ( ( r
         , MonoidalMap Clue ClueState
         )
       , Challenge i r
       )
```

And now that rewards are monoidal, it makes sense to add some monoid
homomorphisms for them:

```{.haskell law="reward/mempty"}
reward mempty = empty
```

and

```{.haskell law="reward/mappend"}
reward (r1 <> r2) = andThen (reward r1) (reward r2)
```

Verify that these are reasonable laws and that they form a monoid homomorphism
with `empty` and `andThen`.

With these changes made, the `Clue` in `step` now sticks out like a sore thumb;
it's the only concrete type in the signature of `step`. And as our analysis
earlier showed, our `Clue` type is isomorphic to a list. Let's just drop in a
list here, but be polymorphic over its contents.

```haskell
data Challenge i r k

clue :: [k] -> Challenge i k r -> Challenge i k r

step
    :: ( IsFilter f
       , Monoid r
       , Commutative r
       , Ord k
       )
    => [k]
    -> Maybe i
    -> Challenge i k r
    -> ( ( r
         , MonoidalMap [k] ClueState
         )
       , Challenge i k r
       )
```

You might be wondering if we can replace `[k]` with some arbitrary monoid ---
and the answer is that we can't. Unfortunately `law:clue/sub` requires us to be
able to rewrite `clue (k1 <> k2)` as `clue k1 . clue k2`, but arbitrary monoids
can't be decomposed like this. On its own, this isn't disastrous, but when
combined with `law:step/clue non-empty` which states that `clue k` must emit
clue `k` in the `seen` state. The only monoid that supports both of these laws
simultaneously is the free monoid: the list.

Our algebra is now parameterized by the clues it can give, the input it takes,
and the rewards it emits. At this point, nothing remains that is
application-specific; the `Challenge` abstraction is entirely reusable and could
be released as a separate library. Even if you expect to be the only
instantiator of this abstraction, enforcing this abstraction boundary is
valuable in ensuring your codebase stays maintainable; firm contracts are our
only defense against entropy, and the abstraction boundaries enforced by
parametricity is as strong as they come.

