## Common Algebraic Components

As you become more comfortable with algebraic design, you'll start to
see the same sorts of patterns emerging time and time again. There is much enthusiasm for tried-and-true design patterns ---
patterns that frequently occur in (procedural) programming contexts in programming circles. You might
not be surprised then to learn that there are analogous *universal algebras,*
which are exceptionally well-studied mathematical objects, and as such, come
"batteries included." It's much like how the language of graphs and graph
traversals empowers a programmer with numerous algorithms, for the minimal
price of showing his problem can be reduced to a graph.

By recognizing these universal algebras in our designs, we will end up with more
powerful abstractions and will have reinvented fewer wheels in the process.

In this chapter, we will take a brief tour of the landscape of universal algebra
--- enough to build an intuition of each of the pieces, and of the value in
recognizing them. The objects we will study here are only a small subset of the
entire field but have been chosen due to the frequency in which they occur in
computational settings. We will look at properties, which are the features of
interest. And afterward, we will explore some algebraic structures, which are specific collections of those properties.


### Properties

Properties are the "interesting" part of universal algebra; they're the laws
that are nearly ubiquitous throughout mathematics, programming, and seem to be
rather intrinsic to the way humans think. These properties are all extremely
familiar, but some appear more often than others, and others are more immediately
visible.

The properties listed here are so commonplace that identifying them gives us
humans an enormous amount of intuition about how we can and cannot interact with
an operation. We have been dealing with these properties --- likely implicitly
--- for our entire lives, which means we have a long history of how to deal with
things that satisfy them.

When designing your algebra, before every proposed constructor, you should stop
and run through the following list of properties. Which of them hold? Which of
them should hold, conceptually? Often you'll find a property that is *nearly*
applicable, and in such cases, it's worth jiggling everything around to see if
you can make the property hold. The result is always a more straightforward and more elegant
design. That's not to say that you should *force* it; if a property obviously
doesn't hold, it doesn't hold, and that's OK. But if at all possible, if
it is a close fit, try relaxing constraints until it does.


#### Associativity {#sec:associative}

The most ubiquitous property of all is *associativity,* which is a fancy word
for saying, "the brackets don't matter." An everyday example of associativity is
addition. Consider the grade-school expression:

$$
1 + 2 + 3 + 4 + 5
$$

If we wanted to assign parentheses to this expression, there are 14 different
ways of doing so! We might choose $1 + ((2 + 3) + (4 + 5))$, or perhaps $(((1 +
2) + 3) + 4) + 5$, or one of several other possibilities. But we don't put in the
parentheses because it doesn't matter; addition is associative, and so any
possible parenthesizing of an expression is equal to any other.

In a programming context, we can rephrase the slogan "parentheses don't matter"
as, "we can put the parentheses wherever is most convenient." Convenience might
mean *algorithmic simplicity,* as in the case of concatenating two linked lists
by attaching the tail of one to the other's head. But convenience might
mean *algorithmic complexity,* like the case of maintaining a balanced binary
tree. To hammer this in, let's consider the following expression for some
function `⋆` which is known to be associative:

```haskell
1 ⋆ 2 ⋆ 3 ⋆ 4
```

Associativity means we can parenthesize this expression however we'd like. If
`⋆` is building a linked list, we'd probably want it to be right-associative:

```haskell
1 ⋆ (2 ⋆ (3 ⋆ 4))
```

But if `⋆` is building a binary tree, we'd prefer the following:

```haskell
(1 ⋆ 2) ⋆ (3 ⋆ 4)
```

The point here is, due to associativity, either of these parenthesizing is just
as satisfactory insofar as the meaning of the program is concerned. Because we can
reassociate at will, it allows our users ample flexibility to build expressions
in whatever way is most convenient for them. An optimizing implementation can take
whatever parentheses the user gives it, and reassociate them into something better.  Such an operation takes $O(n)$, but is amortized down
to $O(1)$ because it only ever needs to be done once.

A particularly compelling application of associativity is splitting the
computation of large expressions into disparate chunks. Because we allow for
arbitrary reassociation of parentheses, these pieces of work can be done by
different nodes. So long as the parts are combined in the correct order, this
final result will be equivalent to having done the work on a single process.

More formally, the law of associativity for some binary operation `f :: A -> A
-> A` is expressed thusly:

```{.haskell law="defn: association"}
∀ (x :: A) (y :: A) (z :: A).
  f x (f y z) = f (f x y) z
```

Associativity comes up with staggering regularity. It's an even bet that a
binary operation is associative. Also, if your algebra
contains an operation that takes a list or array, it can often be "distilled"
down to an associative binary operation --- and should be!

If you have associativity, your function `f` automatically forms a semigroup
(@sec:semigroup). Look for an identity element (@sec:identity) to discover more
structure.


#### Identity {#sec:identity}

A binary operation is said to have an *identity element* (or *identity*, for
short) if there is some distinguished term `e` that doesn't change the results
if operated on. What does that mean? Let's return to the example of addition.
For any number, adding zero to it is just that same number:

$$
x + 0 = x
$$

Here we would say that $0$ is the identity element for addition.
Identities can come in three flavors: true identities, left-identities, and
right-identities. Our zero example is a true identity because you can add zero
on either side without changing the result:

$$
x + 0 = x = 0 + x
$$

But for some operations, they only have identities in certain arguments, which
is often the case when the operation is not commutative (see @sec:commutative.)
As another mathematical example, let's consider the division operation, where
the following rule is true:

$$
x / 1 = x
$$

Thus, our identity for division is one --- but note that this is only true in the
second argument! It is not so in general that `1/x =
x`! Because in division, one acts only as an identity on the right side of the
operation, it is called a *right-identity.* A left-identity is analogous, one
which doesn't change the results only when applied as the left-hand-side
argument.

Formally, we can say an operation `f :: A -> A -> A` has an identity (true
identity) element `e :: A` if the following law holds:

```{.haskell law="defn: identity"}
∀ (x :: A).
  f e x = x = f x e
```

You can drop either side of this three-way equality to get the laws for
left/right-identities.

Identities are valuable because they act as meaningless placeholders. An
identity is the right choice for a value that doesn't yet have its final value, or
when you need to give an argument that corresponds to the default case.

If you have an identity element on your function `f`, check to see if `f` is
associative. If so, you have a monoid (@sec:monoid). Look for an inverse
(@sec:inverse) to discover more structure.


#### Idempotency {#sec:idempotence}

*Idempotency* is the property that doing something twice is the same as doing it
once. By extension, doing it three times is the same as doing it once, ad
infinitum. Think about restarting your computer. Regardless of what state it was
in before you reset it, when it finishes powering back on, it will be in a very
particular condition --- the desktop will be empty of windows, and you will have no
files open. Such an operation is a significant change, but if we were to restart
it again, immediately after it finished booting, the resulting state would
be the same as if we had restarted it only once.

There is a second flavor of idempotency, which is a binary operation that does
nothing when applied to the same argument. For example, `max :: Int -> Int ->
Int` is idempotent:

```haskell
max x x = x
```

In programming, idempotence often shows its face when dealing with variable
assignment, information propagation, and search refinement. Assigning
the same value to a variable twice in a row is the same as doing it once, and
that asking the same thing twice in a game of "twenty questions" doesn't gain
you any new information.

Once you've identified an idempotent system (or explicitly made one), you are
rewarded with what I call "shotgun semantics" --- especially when combined with
commutativity (@sec:commutative.) You can just throw whatever you have at the
system, and let the idempotency laws deal with sorting out the mess. This is
incredibly valuable in systems that have outgoing, user-visible actions, like
an emailing system. Building idempotence into your mailing system means you can
attempt to send emails whenever a relevant action happens, and rely on the laws
to ensure we don't spam the user.

Formally, we can write our two flavors of idempotency as follows. In the first,
a function `f :: A -> A` is said to be idempotent if `lawn:defn: idempotent
func` holds:

```{.haskell law="defn: idempotent func"}
∀ (x :: A).
  f (f x) = f x
```

Similarly, a binary operation `(<+>) :: A -> A -> A` is said to be idempotent
if:

```{.haskell law="defn: idempotent op"}
∀ (x :: A).
  f x x = x
```

If you have idempotency for your function `f`, check to see also if it is
commutative (@sec:commutative) and associative (@sec:associative). If so, you've
discovered a semilattice (@sec:semilattice).


#### Invertibility {#sec:inverse}

*Invertibility* is best known as undo/redo functionality. For any given action,
there is an opposite action such that doing them one after another is equivalent
to having done nothing --- that is to say, equivalent to the identity element.
Invertibility is a property of the *terms* of an algebra --- not to be confused
with the related idea of function inverses which cancel one another.

We can formalize this notion, saying that for some operation `f :: A -> A -> A`,
and `e :: A` such that `e` is an identity for `f`, then there must be a function
`invert :: A -> A` for which the following two laws hold:

```{.haskell law="defn: inverse"}
∀ (x :: A).
  f x (invert x) = e = f (invert x) x
```

```{.haskell law="inverse/inverse"}
∀ (x :: A).
  invert (invert x) = x
```

The term `invert x` is called the *inverse* of `x`, and is a proper object in
its own right --- it's something which can be manipulated and substituted for
any other term in the algebra.

When `f` is associative we can reassociate parentheses on demand, allowing for
elimination of terms on either end of a chain of calls to `f`. Take for example,
the term `f (f x y) z` in the following expression. We can either eliminate the
`x`:

```{.haskell .proof}
  f (invert x) (f (f x y) z))
= -- .via defn: association
  f (invert x) (f x (f y z))
= -- .via defn: association
  f (f (invert x) x) (f y z)
= -- .via defn: inverse
  f e (f y z)
= -- .via defn: identity
  f y z
```

or the `z`:

```{.haskell .proof}
  f (f (f x y) z) (invert z)
= -- .via defn: association
  f (f x y) (f z (invert z))
= -- .via defn: inverse
  f (f x y) e
= -- .via defn: identity
  f x y
```

In this example, we managed to eliminate an `x` that occurred deep within the
call tree to `f`! I am always amazed that such a thing is possible; remember
that this isn't merely a "syntactic trick." At runtime, there will be an actual
memory representation for this `f x (f y z)` structure, and so the fact that we
can eliminate the `x` from here is staggering.

Like with identity elements, inverse elements can be "sided" ---
they can eliminate elements on only one side of the operation.

Invertibility often comes up when dealing with discrete actions (as in
undo/redo), and when working with spatial transformations.

If you have an `invert` function for your function `f` such that `law:defn:
inverse` holds, check to see also if it is associative (@sec:associative) and
has an identity element (@sec:identity). If so, you've discovered a group
(@sec:group).


#### Distributivity {#sec:distributive}

We say two functions *distribute* over one another if we can move one through
the other, that is, change the order we call them. Distributivity is at
the very heart of much of Algebra-Driven Design; much like associativity, it
allows us to shuffle an expression tree into a more convenient form. But
unlike associativity, it will enable us to move entire sub-trees.

By exploiting distributivity, we can often find *canonical forms* of expressions,
which are exceptionally well-organized expressions that satisfy some invariant.
For example, when working through the `Challenge` algebra in
@sec:scavenge-design, we found that `reward` nodes could always be propagated
upwards to the nearest `gate` node. By ensuring that they were invariably there, our
implementation could award them all at once --- in constant time.

Distributivity allows us to hide operational concerns from our users; they can
build whatever term is most convenient for them, and we will silently manipulate
it into canonical form behind the scenes.

It's hard to nail down distributivity precisely with the formal
equational machinery presented in this book; we play rather fast-and-loose with
the terminology here. For our purposes, two functions `f` and `g` distribute
over one another if you can find a law such that:

```{.haskell law="defn: distribute"}
f . g = g . f
```

But this is more of a suggestion than the rule. To pull up some examples from
previous algebras discussed in this book, I would call both of the following
laws distributive:

```{.haskell law="both/andThen/reward"}
∀ (r :: Reward) (c :: Challenge).
  pruned (andThen (reward r) c) =
    andThen (reward r) (pruned c)
```

```{.haskell law="above/beside"}
∀ (a :: Tile) (b :: Tile) (c :: Tile) (d :: Tile).
  above (beside a b) (beside c d) =
    beside (above a c) (above b d)
```

We can factor `law:both/andThen/reward` into two functions that behave properly
like `law:defn: distribute` says they should: `f = pruned` and `g = andThen
(reward r)`, but this is not nearly as straightforward in `law:above/beside`.
Regardless, it's the spirit that counts, and `above/beside` certainly
has the distributive nature.


#### Commutativity {#sec:commutative}

A function which is invariant to the order of its arguments is called
*commutative.* In essence, such functions allow you to move arguments around,
which, combined with associativity, enables complete rewriting of nested
expression trees.

Commutative operations relieve users of needing to worry about ordering or
sortedness and are of particular value when combining work from
non-deterministic processes. While a semigroup requires work to be recombined in
the right order, commutative operations do not. They can accumulate
results as quickly as possible since any ordering of the data is acceptable.

Formally, a function `f :: A -> A -> A` is said to be commutative if the
following law holds:

```{.haskell law="defn: commutative"}
∀ (x :: A) (y :: A).
  f x y = f y x
```

If your function `f` is commutative, check for idempotence (@sec:idempotence) and
associativity (@sec:associative). If everything's all there, you've got a
semilattice (@sec:semilattice). If it's associative, check for an identity
element (@sec:identity) to discover a commutative monoid. If it further
has an inverse (@sec:inverse), you've found an abelian group.


#### Annihilation {#sec:annihilative}

Finally, we come to our last common property: *annihilation.* A function `f` can
be annihilated if there exists some distinguished element `a` --- called the
annihilator --- which forces the output of `f` to itself be `a`. An example will
help clarify. When dealing with grade-school multiplication, we know that any
number multiplied by zero is zero:

$$
0 \times n = 0 = n \times 0
$$

Here, zero is the annihilator of multiplication, because it forces the output to
itself be zero.

Annihilators act as "black holes" of a sort; they tend to propagate upwards in a
call stack, eating as much as possible. This is particularly so when multiple
operations in the algebra share the same annihilator. The result is always
greatly simplified afterward. In programming, annihilators come up in the
context of unrecoverable errors, transactional rollbacks, and other things of
an "undo" sort of nature.

Formally, a function `f :: A -> A` (which could be a partially applied version
of a higher-arity function) has an annihilator `a :: A` if the two of them
interact in the following way:

```{.haskell law="defn: annihilation"}
f a = a
```


### Structures

Specific patterns appear time and time again when you start looking at
computation from the algebraic perspective. These patterns are formed by a set
of constructors, related to one another by reoccurring laws. Over time, these
patterns were extracted and generalized from their constituent algebras,
becoming objects of study in their own right, which we call *algebraic
structures.*

The best-known algebraic structure in computer science is the graph. By spotting
the graph structure inherent in a design, we can pull in the entire field of
graph theory to aid us. New practitioners of graph theory are always shocked by
how powerful the approach is; challenging problems often melt away under the
lens of graph theory.

Graph theory's value is twofold: it gives us a new vocabulary for thinking about
problems. Better yet, it gives us access to every tool written for working with graphs. These tools might come in the form of
executable code --- like libraries for doing graph traversals --- but they also
might come in the form of *theorems.* For example, showing a problem is
isomorphic to the traveling salesman problem immediately informs us that we can
never get an optimal answer.

But graphs aren't the only valuable algebraic structure. There are a host of
others, each as helpful as graphs --- or perhaps even more so.

An unfortunate side effect of great generality is always the difficulty in
immediately seeing usefulness; here too, you might find yourself thinking, "who
cares?" or "what is any of this good for?" There is no solution to this problem
other than simply to wrestle with the concepts. As you work, regularly ask
yourself if you've run across any algebraic structures of late. Before the
intuition has set in, it's acceptable to work "equationally" --- that is, to
check manually whether the required laws hold. If so, encourage yourself to
replace your design's specialized vocabulary with the generalized
language of the algebraic structure. More often than not, this change will
allow for further generalization; perhaps now there is no more of your original
vocabulary left, which allows your design to be *entirely polymorphic* in the
concrete instantiation of its algebraic structures. If you look for
opportunities like these and take them whenever possible, you'll be amazed at
how quickly you gain an intuition for these unreasonably abstract concepts.


#### Semigroups {#sec:semigroup}

The simplest algebraic structure is the *semigroup* which consists of a single
binary operation. In Haskell, we write the semigroup operation as `(<>)`, with
type:

```haskell
(<>) :: T -> T -> T
```

and pronounce it as "append" or "multiply."[^why-multiply]

[^why-multiply]: Most of the algebraic structures were discovered by
  mathematicians attempting to generalize arithmetic and the usual algebra over
  numbers. For better or worse, the terminology of abstract algebra reflects
  this history today.

Semigroups are required to fulfill only one equation, namely associativity
(@sec:associative):

```{.haskell law="semigroup:associative"}
∀ (x :: T) (y :: T) (z :: T).
  x <> (y <> z) = (x <> y) <> z
```

That's it! The requirements for being a semigroup are non-demanding, and
therefore, semigroups are extremely widespread. Some semigroups you're already
familiar with include list and string concatenation, addition, multiplication,
the boolean AND and OR operations, the `over` color mixing operation, the `min`
and `max` functions, picking the first and last element in a series, and
function composition itself. Additionally, things like combining disparate
pieces of configuration usually form a semigroup. Semigroups appear everywhere!

The intuition behind a semigroup is exactly what was described in
@sec:associative --- namely, it removes the need for an explicit "order of
operations." It's OK to squish together a bunch of things no matter how they're
parenthesized.


#### Monoids {#sec:monoid}

A *monoid* gives additional structure to a semigroup; enriching it with a
distinguished identity element (@sec:identity), traditionally called `mempty` in
Haskell, and the "neutral element", "identity" or "unit"[^why-unit] in more
mathematical circles. Every monoid is a semigroup, but not every semigroup is a
monoid.

[^why-unit]: The names "identity" and "unit" come directly from the fact that
  $1$ forms the `mempty` element for multiplication.

Monoids are required to satisfy three laws --- the associativity inherited from
semigroup:

```{.haskell law="monoid:associative"}
∀ (x :: T) (y :: T) (z :: T).
  x <> (y <> z) = (x <> y) <> z
```

and laws stating that `mempty` forms an identity on both the left and right:

```{.haskell law="monoid:identity"}
∀ (x :: T).
  x <> mempty = x = mempty <> x
```

Due to this identity element, monoids are slightly rarer than semigroups and
significantly more exciting. The `mempty` operation provides a suitable
choice for a default value because it is entirely "inert" when interacting
with other elements. It is this property that generalizes our grade-school
intuition that zero is a suitable default number --- but notice that zero is a
*disastrous choice* for a default if you intend to multiply your numbers
together! Monoids neatly solve this problem, generalizing the neutral element to
something that is always safe to multiply in.

To show a monoid, we must give both its multiplication operation and
its identity element. For example, the integers form a monoid under $(\times,
1)$, and another monoid under $(+, 0)$.


Exercise

:  Most of the semigroups we saw in @sec:semigroup also form monoids. For each
   semigroup described there, determine if it has an identity element or not.


#### Groups {#sec:group}

*Groups* further generalizes monoids (and therefore semigroups), endowing them
with invertibility (#sec:inverse) of every element. In Haskell, this is given by
way of a function:

```haskell
invert :: T -> T
```

satisfying the following laws --- again, with the first three inherited from the
underlying monoid:

```{.haskell law="group:associative"}
∀ (x :: T) (y :: T) (z :: T).
  x <> (y <> z) = (x <> y) <> z
```

```{.haskell law="group:identity"}
∀ (x :: T).
  x <> mempty = x = mempty <> x
```

and

```{.haskell law="group:invertibility"}
∀ (x :: T).
  x <> invert x = mempty = invert x <> x
```


Exercise

:  Prove the corollary that `invert mempty = mempty`.


Groups arise whenever there is a notion of "undo." This might be in the context
of your text editor, or as a special case, whenever you're working with spatial
transformations. Outside of these contexts, groups don't arise exceptionally often.


#### Semilattices {#sec:semilattice}

Rather than move up to a monoid, we can instead extend semigroups differently. By enhancing a semigroup with idempotency (@sec:idempotence) and
commutativity (@sec:commutative), we obtain a *semilattice.* Semilattices are
therefore required to satisfy the following three laws:

```{.haskell law="semilattice:associative"}
∀ (x :: T) (y :: T) (z :: T).
  x <> (y <> z) = (x <> y) <> z
```

```{.haskell law="semilattice:idempotent"}
∀ (x :: T).
  x <> x = x
```

and

```{.haskell law="semilattice:commutative"}
∀ (x :: T) (y :: T).
  x <> y = y <> x
```

Semilattices generalize the notion of "refinement by evidence." Under this lens,
`semilattice:idempotent` says you don't learn anything new by looking at the
same evidence again, and `semilattice:commutative` says that the sum of
knowledge from two pieces of evidence is independent of the order in which you
look at them. As such, semilattices are extremely useful in the context of
propagator networks. Additionally, they form the theoretical underpinning of
eventually-correct systems, and conflict-free replicated data types.


#### Functors {#sec:functor}

To switch gears entirely, let's look at *functors.* Functors are likely the most
important algebraic structures in computing as they generalize the idea of a
data structure. The one thing that data structures have in common is that they
act as containers for other types; a set, a trie, and an optional value are all
containers of something, but that's about as far as the similarities go. Each
supports completely different operations, and we can't even necessarily get a
value *out* of the optional.

But what we can do in all cases is change the type of contained values by
walking its structure and running a function on every value inside (of which
there may be zero, or possibly even an infinite number!) This is more valuable
than it seems; it allows us to abstract away the common transformative
`foreach` pattern often found in procedural programming.

To have a functor, we must first have a type that is parameterized by
another, and an operation:

```haskell
fmap :: forall a b. (a -> b) -> F a -> F b
```

Additionally, functors must satisfy the following laws. First, `fmap` must
map the identity function to the identity function:

```{.haskell law="fmap/id"}
fmap id = id
```

and additionally must preserve the composition of functions:

```{.haskell law="fmap/compose"}
∀ (g :: b -> c) (f :: a -> b).
  fmap (g . f) = fmap g . fmap f
```

If you squint, you'll notice that `law:fmap/id` and `law:fmap/compose` look a great
deal like a monoid homomorphism.

In Haskell, `law:fmap/compose` holds if and only if `law:fmap/id` does, which
saves us the work of checking it. Because the types `a` and `b` are universally
quantified in the type of `fmap`, the only thing that can possibly type check is
to apply the function to every value of type `a` contained within. The law
`law:fmap/id` forces us to not fiddle with anything *else* while we're in there.

Most parameterized types you've ever come across will form functors ---
functions, lists, `Maybe`, `Either`, matrices, tuples, infinite streams --- you
name it. Even bizarre containers like the single-element container and the zero
element container both form functors!

Look for a functor whenever your type contains another, but when most of your
operations operate over the structure of the data, rather than the values
inside.


#### Applicative Functors {#sec:applicative}

The final algebraic structure we will look at (though this list is by no means
comprehensive) is the *applicative functor.* An applicative functor is a functor
that has been enriched with the ability to create a value of the "containing",
and with the ability to combine two containers together.

Although this is not its usual presentation, we can express these ideas in
Haskell by way of two functions:

```haskell
unit :: T ()
zap  :: T a -> T b -> T (a, b)
```

which, in addition to the functor laws, must also respect:

```{.haskell law="fmap/fst/zap"}
∀ (x :: a).
  fmap fst (zap x unit) = x
```


```{.haskell law="fmap/snd/zap"}
∀ (x :: a).
  fmap snd (zap unit x) = x
```

as well as be associative:

```{.haskell law="fmap/snd/zap"}
∀ (x :: a) (y :: b) (c :: z).
  fmap reassoc (zap x (zap y z)) = zap (zap x y) z
```

where

```haskell
reassoc :: (a, (b, c)) -> ((a, b), c)
reassoc (x, (y, z)) = ((x, y), z)
```

Fundamentally, these laws govern what "combining two containers" means. Consider
the two following applicative functors over lists:

```haskell
unit :: [()]
unit = [()]

zap :: [a] -> [b] -> [(a, b)]
zap = cartesianProduct
```

and

```haskell
unit :: [()]
unit = repeat ()  -- ! 1

zap :: [a] -> [b] -> [(a, b)]
zap = zip  -- ! 2
```

where the `repeat` function at [1](Ann) creates an infinitely long list, and
`zip` at [2](Ann) combines two lists element-wise, truncating to whichever list
is shorter.


Exercise

:  Show that both `([()], cartesianProduct)` and `(repeat (), zip)` form
   applicative functors over lists.


In Haskell, applicative functors are usually expressed in terms of the
equivalent operations `pure` and `(<*>)` --- pronounced "ap" --- given by:

```haskell
pure :: a -> T a
pure a = fmap (const a) unit

(<*>) :: T (a -> b) -> T a -> T b
tf <*> ta = fmap (uncurry ($)) zap tf ta
```

This `(<*>)` operation enables a particular Haskell idiom, which lifts function
application over "pure values" into function application over applicative
functors --- thus the name. Examples of this are scattered throughout
@sec:scavenge-design and @sec:scavenge-impl, where we lift expressions of the
form:

```haskell
step i (both c1 c2) = both <$> step i c1 <*> step i c2
```


