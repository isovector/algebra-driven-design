## Effective QuickSpec {#sec:quickspec}

QuickSpec is a tool introduced by @smallbone_quick_2017, which can independently
discover the algebraic laws of an implementation. This has obvious utility when
reverse-engineering the algebra from a legacy project, but is surprisingly
useful when working with greenfield initiatives. Before diving into how
to use QuickSpec to get real work done, let's first examine what it is, how it
works, and roughly how to use it.

At its core, QuickSpec uses a handwritten *signature*, consisting of
constructors and types, and uses it to generate possible syntactic terms. It
then uses QuickCheck on every pair of produced terms, to determine if they are
equal. If so, QuickSpec has found a law and will share it with you. Let's look
at an example:

```haskell
import QuickSpec

main :: IO ()
main = quickSpec
  [ con "+" (+)
  , con "*" (*)
  , con "0" 0
  , con "1" 1
  ]
```

When we run `main`, QuickSpec immediately produces the following output:

```text
== Functions ==
(+) :: Integer -> Integer -> Integer
(*) :: Integer -> Integer -> Integer
  0 :: Integer
  1 :: Integer

== Laws ==
  1. x * y = y * x
  2. x + y = y + x
  3. x * 0 = 0
  4. x * 1 = x
  5. x + 0 = x
  6. (x * y) * z = x * (y * z)
  7. x * (y + y) = y * (x + x)
  8. (x + y) + z = x + (y + z)
  9. x * (y + 1) = x + (x * y)
 10. x * (y + (y + y)) = y * (x + (x + x))
 11. (x * y) + (x * z) = x * (y + z)
```

Not bad! QuickSpec has discovered the commutativity of multiplication and
addition, their respective identities, associative laws, and the fact that
multiplication distributes over addition. And all of this came for free; we
simply needed to tell QuickSpec which functions we were interested in, and it
did the rest. As you can imagine, this has quite the potential when being used
to probe unfamiliar codebases, and more interestingly, for *familiar* codebases.

Of course, this is not magic. In our first experience with QuickSpec, we were
fortuitous in that a generator already existed for numbers, and that
observational equality is simple and already implemented for us. For more
exciting algebras, we will need to provide both of these pieces.


### Signatures

A signature consists of an explicitly enumerated list of constructors and a
list of types that we're interested in investigating. Constructors are given
via the `con` function:

```haskell
con :: Typeable a => String -> a -> Signature
```

The `con` function takes the name of constructor and the corresponding Haskell
function. *This duplication of name and function is a common source of typos;*
be warned that it's easy to copy a line, and then change only the name,
leaving the second parameter unmodified. *If you see counterintuitive
output from QuickSpec, this is the first thing to verify.* By requiring the
`Typeable` constraint, `con` can inspect the type information of its
second parameter and only construct well-typed terms in its laws.

For monomorphic functions, `con` works without any further refinements. But when
working with polymorphic functions, we have a choice to make. If we'd like to
maintain the polymorphism, we will need to explicitly put a type signature on
our implementation, replacing its type variables with QuickSpec's special
polymorphic type markers. For example, the empty list has type:

```haskell
[] :: [a]
```

If we'd like to expose the empty list to QuickSpec, we can use the signature:

```haskell
con "[]" ([] :: [A])
```

Note the capital `A` in the signature, compared to the lowercase `a` in the
actual type of `[]`. QuickSpec uses the special `A` type[^and-other-markers] to
indicate genuine polymorphism, and it will handle this case correctly when
generating terms.

[^and-other-markers]: QuickSpec also defines `B`, `C`, `D`, and `E` as other
  available polymorphic type markers. Each of these corresponds to a unique type
  variable --- for example, `const :: a -> b -> a` can be exposed to QuickSpec
  via `con "const" (const :: A -> B -> A)`.

By default, QuickSpec is aware of integral numbers, characters, strings,
booleans, sums, products, and lists of such. QuickSpec will only
generate and evaluate terms of these types --- including in its instantiation of
polymorphic types. QuickSpec will tolerate other types present in signatures,
but it will complain bitterly about them at runtime. You will often run into
warnings:

```text
WARNING: The following types have no 'Arbitrary' instance declared.
You will not get any variables of the following types:
  MyType

WARNING: The following types have no 'Ord' or 'Observe' instance declared.
You will not get any equations about the following types:
  MyType
```

We will discuss these warnings in further detail later, as they showcase a
rather interesting feature of QuickSpec. But for now, it suffices to use the
`mono` signature to assuage QuickSpec's concerns. The invocation of the
signature looks like this:

```haskell
mono @MyType
```

QuickSpec uses the `mono` signature to get its hands on the `Arbitrary`
and `Ord` instances for your type. It uses `Arbitrary` to generate terms in its
attempts to prove laws, and uses `Ord` to compare the results for equality. In
principle, you only need the `Arbitrary` instance if your signature has
functions that consume `MyType` values and the `Ord` instance for functions
that produce `MyType` values. The `mono` signature assumes you'll be doing
both, but the library supports finer-grained controls if defining either of
these instances is too onerous for you.


### Motivating QuickSpec

It might not be immediately obvious what value QuickSpec brings to the table, so
before going on, let's work through the life cycle.


#### QuickSpec for Designing Greenfield Projects

Imagine you're designing a new system. Rather than diving directly into a
performant implementation, you decide to spend some time figuring out the
algebra. After some thinking, you come up with a set of constructors, with a hazy
idea of what exactly their semantics should be. These constructors can be turned
immediately into a naive implementation, one in which there has been no thought
put into performance, but that exists purely as a proof of concept.

Rather than proving the semantics in your mind hold of the system at large, and
rather than explicitly writing tests to check the laws, you can instead throw
QuickSpec at the problem. It will automatically discover laws about your
algebra, and you can then inspect each by hand. Many of the laws it finds
will be reasonable, but ones you might not have anticipated. In this case,
QuickSpec has taught you something about your system. Other times, the laws
might be surprising, which is often indicative of a bug in your implementation,
or worse, your choice of constructors. Getting feedback of this sort before
you've done significant implementation work usually pays for itself the first
time you run into issues; the amount of time you spend over a lifetime
prototyping with QuickSpec is roughly on par the amount of time you'd have
wasted implementing a wrong specification *for a single project.*

I often use QuickSpec interactively in my REPL when working on my
implementation. Sometimes I'll get the intuition that there's a performance
optimization when functions `f` and `g` are used together; for example, maybe
they both reduce to some other function `h` in the presence of `x`. The exact
details aren't significant. In cases like these, I like to make a little QuickSpec
signature with `f` and `g`, put the rest of my algebra in the background, and
then turn the crank and see what comes up. If QuickSpec discovers any helpful
laws, I'll jot them down in my notebook and add them to the test suite. It's an
excellent way of finding particular sorts of laws!

Also of particular interest in QuickSpec's output are laws that you'd expect to
be there, but which aren't. This is often indicative of an incorrect mental
model; usually, it means that some of the semantics you're ascribing to your
system do not hold in general.

After you've fine-tuned your algebra, ensuring that the laws come out as you
expect, QuickSpec can emit QuickCheck properties. Because the
algebraic laws are concerned only with your system's public interface, you
can now swap out the naive implementation with something more thoughtful and
performant. In doing so, your new property-based tests become *regression
tests,* ensuring your performant implementation has the same observable results
as the naive, easy-to-get-right version. By doing your reasoning over the simple
implementation, we can take all insights learned and move them to the more
complicated domain. Such a test-suite gives us great confidence that we haven't
introduced any subtle bugs while optimizing performance.


#### QuickSpec for Analyzing Existing Projects

QuickSpec isn't only useful for designing new systems. It can also be used to
great effect in understanding codebases you're unfamiliar with. By running
QuickSpec over existing code, you can gain an understanding of its
publicly-observable semantics, without needing to dig through the low-level
implementation. Not only is this valuable in building your understanding, but
the totality of results from this exploration is unlikely to be known by
*anyone* --- original authors of the codebase included.

Nor does this exploration result in ephemeral knowledge; the QuickSpec output is
useful directly as documentation, while the massaging required to get QuickSpec
to run can immediately be used to implement property-based regression tests.

Coming onto a new project is an excellent opportunity to see with fresh eyes;
QuickSpec allows you to use those fresh eyes to uncover bugs,
improve documentation, and improve test coverage. Coming onto a project with
this amount of immediate input is a surefire way to make a good impression. And
all of this for the price of understanding that you'd need to pay anyway, likely
in a way that is more conducive to learning than reading implementations could
ever be.


### Background Signatures

As brilliant as it is, QuickSpec is a good reminder of just how stupid computers
can be. Consider the following signature over integers:

```haskell
[ con "sum" $ sum @Integer
, con   "+" $ (+) @Integer
]
```

It seems reasonable to expect QuickSpec to find that `sum` is a monoid
homomorphism; that is to say, that the following laws hold true:

```haskell
  1. sum [] = 0
  2. sum (xs ++ ys) = sum xs + sum ys
```

However, QuickSpec will not find these laws, due to not having `0`, `[]`, or
`(++)` in its signature. These are clearly interesting constructors that no
human would forget to consider when deriving laws by hand, and this is a
reminder that QuickSpec isn't magic. It can only work with what we tell it! The
naive fix is to add these constructors to our signature, which produces the
following laws:

```text
== Laws ==
  1. 0 = sum []
  2. x + y = y + x
  3. x + 0 = x
  4. xs ++ [] = xs
  5. [] ++ xs = xs
  6. sum (xs ++ ys) = sum (ys ++ xs)
  7. (x + y) + z = x + (y + z)
  8. (xs ++ ys) ++ zs = xs ++ (ys ++ zs)
  9. sum xs + sum ys = sum (xs ++ ys)
```

Our monoid homomorphism is now derived but at the expense of several other
irrelevant laws, for example, equations about addition itself. That's not to say
that these laws aren't interesting, but they're clearly out of the scope of our
concern here. What we'd like is to weed out any laws which don't reference the
`sum` constructor --- and QuickSpec allows us to do this with the `background`
signature. A background is a signature that is necessary for deriving interesting
laws, but laws about which we are not interested in on their own merit. We can
move all of our constructors other than `sum` into the background:

```haskell
[ con "sum" (sum @Integer)
, background
  [ con    "+" $ (+) @Integer
  , con    "0" $ (0 :: Integer)
  , con "(++)" $ (++) @A
  , con   "[]" $ ([]) @A
  ]
]
```

Rerunning QuickSpec on this new signature produces much better output:

```text
== Functions ==
 (+) :: Integer -> Integer -> Integer
  [] :: [a]
   0 :: Integer
(++) :: [a] -> [a] -> [a]

== Functions ==
sum :: [Integer] -> Integer

== Laws ==
  1. sum [] = 0
  2. sum (xs ++ ys) = sum (ys ++ xs)
  3. sum xs + sum ys = sum (xs ++ ys)
```

You'll notice that there are now two function blocks in our put; the first
describes the constructors in our background --- so we know what's being
considered when generating laws --- while the second lists the actual functions
we're interested in. Our resulting laws now all mention `sum`, just as
we were hoping!

What's also interesting is the laws that we *aren't* given. For example, `sum xs
+ 0 = sum xs` is a true law that mentions `sum`. But this is a statement about
addition, namely that `x + 0 = x`. Behind the scenes, QuickSpec is still going
through the background and deriving all of its laws. Later, when QuickSpec
generates a term of the form `sum xs + 0 = sum xs`, it notices that this is just
a special case of the more general law of addition and elides it.  The background
is useful for more than just hiding constructors; it also hides any laws derived from those constructors alone.

Judicious use of the `background` signature allows you to refine QuickSpec's
output, filtering for only relevant pieces of information. When discovering
specifications for a real algebra, you'll find yourself moving functions in and
out of the background frequently, clarifying your understanding of how
particular constructors work together and in isolation.


### Predicates

Sometimes laws are only true under some circumstances. Consider the case of map
insertion, given by:

```haskell
insert :: Ord k => k -> v -> Map k v -> Map k v
```

Prima facie, we should expect insert to distribute over itself, namely that
`insert k v (insert k2 v2 m) = insert k2 v2 (insert k v m)`, since it doesn't
matter which order keys are inserted into a map. However, this list is notably
missing from QuickSpec's output:

```text
== Laws ==
  1. insert k v (insert k v2 x) = insert k v x
  2. insert k v (insert k2 v x)
       = insert k2 v (insert k v x)
```

These laws suggest what's going wrong; inserting the same key twice with
different values is the same as inserting it once with the final value.
Our desired distribution law is true only when `k /= k2`. QuickSpec is capable
of dealing with conditional laws like these by way of the `predicate` signature:

```haskell
predicate :: String -> (a -> Bool) -> Sig
```

Introducing a predicate is the same as introducing a constructor to QuickSpec,
except that it will also attempt to find laws conditional on the predicate being
true. In this case, adding the following signature is sufficient to discover our
desired law:

```haskell
predicate "/=" $ liftC @(Eq A) $ (/=) @A
```

resulting in the output:

```text
== Laws ==
  1. insert k v (insert k v2 x) = insert k v x
  2. insert k v (insert k2 v x)
       = insert k2 v (insert k v x)
  3. k2 /= k =>
       insert k v (insert k2 v2 x)
         = insert k2 v2 (insert k v x)
```

Evaluating predicates can often increase QuickSpec's running time
asymptotically, so they are best used in small numbers to discover conditional
laws you care about, and then being removed again from the signature.


### Naming Variables

By default, QuickSpec isn't very imaginative with the names it gives variables.
For types you provide to it via `mono`, QuickSpec will name variables, in
order, "x", "y", "z", "w", "x2", "y2", and so on. It quickly becomes difficult
to parse laws with several different types. Instead, we can ask QuickSpec to give more suggestive
identifiers to the variables it generates. By using the `var :: [String] -> Proxy a
-> Sig` signature, we can give a list of better identifiers for a type.
QuickSpec will enumerate the given list, and if it needs more identifiers, it
will go through the list again, adding a number at the end.

For example, we can name regular expression variables `rx` with the following
signature:

```haskell
vars ["rx"] $ Proxy @Regex
```

which results in laws of the form:

```text
== Laws ==
  1. match rx xs && match rx2 ys
       => match (cat rx rx2) (xs ++ ys)
```

It's much easier to spot the regular expressions above at a glance.

We can combine the `mono` and `vars` signatures into one, defining our types and
their variable names at the same time by using the `monoVars` signature:

```haskell
monoVars @Regex ["rx"]
```


### Observing Equalities

Recall that for the algebras advocated in this book, equality is meaningless
outside the context of an observation. For implementations that give rise to a
canonical syntactic form, equality can be exposed to the end-user via this
syntactic equality. Unfortunately, with types for which this is not the case,
we'd still like to observe equality in our tests. This equality test is likely
costly, so it is prudent not to leak it as part of the public
interface; library users rightly expect that an equality test is cheap. By
exposing it, we are inviting poor performance.

Instead, we'd like to introduce a test-only notion of equality for our algebras.
Remember that two terms are equal if every possible algebraic observation over
them is equal. QuickSpec allows us to override the idea that Haskell's `Eq`
typeclass describes equality, instead, it gives us access to:

```haskell
class (Arbitrary test, Ord outcome)
      => Observe test outcome a where
  observe :: test -> a -> outcome
```

An `Observe test outcome a` instance allows us to show that two values of `a`
are observationally equal by mapping to `outcome`, given some input `test`. To
illustrate this better, we can say that two regular expressions are equal if
they match[^better-to-consume] the same set of strings. In this case, our `test`
would be an arbitrary `String` to match over, and the `outcome` is a `Bool` ---
the result of matching.

[^better-to-consume]: It's better to show that two regular expressions consume
  and match the same *amount of input*, rather than reducing it to a binary
  yes-or-no result. Given arbitrary strings and regexes, it's very likely that
  they will not match entirely, but they have significantly better chances of
  matching some input. All of this is true but distracts from the example.

```haskell
match :: Regex -> String -> Bool

instance Observe String Bool Regex where
  observe str regex = match regex str
```

It's important to note that if we had more observations over `Regex`, that we
include those in the definition of `observe` as well: equality requires every
observation to agree.

Finally, we can inform QuickSpec that we'd like to use observational equality
rather than standard `Eq`-equality by using the `monoObserve` signature
instead of `mono`.

```haskell
monoObserve @Regex
```


### Creating QuickCheck Tests

When we're eventually happy with the laws that QuickSpec has found for us, the
question becomes how we can transform them into regression property tests for
QuickCheck. While it's certainly possible to copy and paste the output directly,
quantifying the correct binders and attaching meaningful labels to each,
thankfully, there is a better way. QuickSpec comes with an option to do this out
of the box.

By adding the `withPrintStyle ForQuickCheck` signature, QuickSpec's output will
be a value of type `[(String, Property)]`. For example, if we expose only
addition in our signature, we will be rewarded with the following code:

```haskell
quickspec_laws :: [(String, Property)]
quickspec_laws =
  [ ( "x + y = y + x"
    , property $
        \(x :: Int) (y :: Int) -> (x + y) === (y + x))
  , ( "(x + y) + z = x + (y + z)"
    , property $
        \(x :: Int) (y :: Int) (z :: Int) ->
          ((x + y) + z) === (x + (y + z)))
  ]
```

It's not the world's most beautiful code, but it gets the job done, and best of
all, it requires no babysitting. Don't be afraid to merge the resulting tests from
a few separate runs of QuickSpec with different signatures!

One mysteriously missing feature in QuickCheck is the ability to run multiple
property tests, but we can get by with the `traverse_` function:

```haskell
main :: IO ()
main =
  traverse_
    (quickCheck . uncurry counterexample)
    quickspec_laws
```

If you already have more of a test harness in place, like that provided by the
`hspec` library, you can instead run the generated tests like this:

```haskell
spec :: Spec
spec = do
  describe "quickspec laws" $
    traverse_ (uncurry describe) quickspec_laws
```


### Variable Usage

QuickSpec is very keen to find laws, and by default, will often find inanely
contrived laws. For example, if we were testing `ifThenElse :: Bool -> Bool ->
Bool -> Bool`, if will find the law:

```text
== Laws ==
  1. ifThenElse x x x = x
```

which while being technically true, begs the question *who cares?* QuickCheck
prefers to reuse variables if it can, and in doing so finds the law above --- an
unhelpful corollary of the two laws we really care about:

```text
== Laws ==
  1. ifThenElse True x y = x
  2. ifThenElse False x y = y
```

We can tune QuickSpec to give us only those laws which use variables exactly
once, by way of the `variableUse` signature:

```haskell
variableUse :: VariableUse -> Proxy a -> Signature
```

The `Proxy` argument here is a remnant of QuickSpec's old design in which type
applications couldn't be used to specify type arguments. We can require
QuickSpec to generate only laws with linear usage of booleans by adding the
following signature:

```haskell
variableUse Linear $ Proxy @Bool
```

Setting linearity will drastically prune laws from your QuickSpec output. This
is often helpful when manually inspecting the discovered equations, but should be
disabled when using QuickSpec to generate regression tests.


### Debugging QuickSpec Output

As great of a technical tool as QuickSpec is, it's not without its share of
usability issues. This section serves to warn you about potential problems
you'll run into, and how to avoid letting them take up an afternoon of your
time.


#### Common Warnings

Occasionally when you run `quickSpec`, you'll see warnings of the form:

```text
WARNING: The following types have no 'Arbitrary' instance
  declared.
You will not get any variables of the following types:
  Foo

WARNING: The following types have no 'Ord' or 'Observe'
  instance declared.
You will not get any equations about the following types:
  Foo
```

In most cases, this is symptomatic of forgetting to add `mono @Foo` or
`monoObserve @Foo` to your signature. However, as of QuickSpec version 2.1.4 ---
the latest at time of writing --- there is a slight bug in how QuickSpec deals
with polymorphism. Consider the following signature, written for our tile
algebra in @sec:tiles:

```text
== Functions ==
 flipH :: Tile a -> Tile a
 flipV :: Tile a -> Tile a
beside :: Tile a -> Tile a -> Tile a
 above :: Tile a -> Tile a -> Tile a
behind :: Tile Color -> Tile Color -> Tile Color
    cw :: Tile a -> Tile a
   ccw :: Tile a -> Tile a
```

Everything here looks fine, and we correctly added `monoObserve @(Tile Color)`
to the signature. But when running it, QuickSpec complains with the following errors:

```text
WARNING: The following types have no 'Arbitrary' instance
  declared.
You will not get any variables of the following types:
  Tile Int

WARNING: The following types have no 'Ord' or 'Observe'
  instance declared.
You will not get any equations about the following types:
  Tile Int
```

What? Where did `Int` come from? Why is QuickSpec asking about that? In short,
it's a bug. Out of the box, QuickSpec attempts to default all polymorphic types
to `Int`[^unless-default-to] and for whatever technical implementation reason,
this warning leaks out. Even though QuickSpec will never need or even try to
instantiate a `Tile Int`, it will still complain about it. If you run into this
warning, it's nothing to worry about.

[^unless-default-to]: This behavior can be changed via the `defaultTo :: Proxy a -> Signature` signature.


#### Insane Laws

Sometimes QuickSpec will produce insane laws. For example, it once
tried to convince me that

```haskell
(x * x) * (x + x) = x + x
```

when `x` is an integer. This arithmetic is not true in general, but is true for
$x=-1$, $x=0$, and $x=1$. What went wrong? I had accidentally added
`withMaxTests 5` to my signature, rather than my intended `withMaxTermSize 5`.
I intended to limit QuickSpec's search space down to laws containing five different terms, but instead, I asked it to require only *five
passing tests* to be convinced the law is true. The crazy law above is a symptom of
getting unlucky with our generated values.

Similarly, we can see the equivalently bizarre behavior when accidentally instantiating
polymorphic constructors at types with few inhabitants. This is equivalent to a
hash-collision; on small types, you are more likely to generate the same values,
because there are not many to pick from. Ensure you're not accidentally
defaulting your type variables to `Bool` or some other small type!


#### Poorly-Typed Laws

Sometimes QuickSpec will generate seemingly poorly-typed expressions, like when
it told me map insertion follows the law `Map.insert k v (Map.insert v k z) =
Map.insert v k (Map.insert k v z)`. My brain cannot even attempt to
verify this law because it's unquestionably a type error. QuickSpec is attempting to
use a key as a value, and vice versa!

Again, this is caused by me asking too much of QuickSpec. It doesn't deal in
polymorphic terms, but instead instantiates each type variable, and chooses
integers by default. As far as QuickSpec is concerned, `insert` has the type
`Int -> Int -> Map Int Int -> Map Int Int`, and thus swapping keys and values is
fair game.

When investigating laws that seem to be poorly typed, double-check the function
block at the top of QuickSpec's output to ensure you both agree on the type of
everything involved.


#### Unsimplified Laws

Similarly, QuickSpec can sometimes show laws which should simplify further, but
it will simply refuse to. For example, when looking for laws about the
function `zip :: [a] -> [b] -> [(a, b)]`, it found the following law:

```haskell
 1. zip xs [] = zip ys []
```

Instead, what it should have found is that `zip xs [] = []` --- that the length
of the zip will always be equal to the shorter of the two lists. But in this
case, QuickSpec simply would not come up with the correct law. Why not? It turns
out that I had over-specialized the type of `[]` when adding it to my signature!
Rather than giving it type `[A]`, I gave it type `[Int]`. In the case above,
things start to make sense; QuickSpec instantiates the argument to `zip` at
`[Int]`, but the result needs to be at type `[(Int, Int)]`. Since as far as
QuickSpec is concerned, there is no empty list for this type, it gives us the
next best thing: some other expression that is also equal to the empty list.

A similar thing often happens when working with predicates. If you forget to add
the `True` constructor, QuickSpec will cleverly come up with an equivalent
tautological value: usually something like `isEmpty empty`. Although it can be
funny to see the hoops it jumps through, the solution is just to give QuickSpec
the constructors it needs.

Whenever you see a law of the form `f x = f y`, suspect that you forgot some
constructors, or that you instantiated the constructors too specifically. A good
solution here is to give the value `f x` an explicit `con` signature with a
descriptive name.

