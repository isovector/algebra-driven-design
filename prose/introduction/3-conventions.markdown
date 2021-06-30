### Conventions

This book uses a few conventions throughout its pages. While it's not necessary
to discuss the full "hows and whys" here and now --- I'm sure you're hungry to
jump into the thick of Algebra-Driven Design --- we will need to cover the
basics.

#### Why Haskell?

**If you are already comfortable with the basic syntax of Haskell, and roughly
how its type system works, feel free to skip to @sec:laws.**

This book presents its examples in Haskell and uses many common Haskell idioms. This might seem like a strange choice to many
readers --- why have I not picked a better-known language? Haskell is an odd
duck of a language: it doesn't do object-oriented programming; it doesn't have
mutable variables; its type-system is famously complicated, and its syntax is
not inspired by C. So why use it?

First and foremost, Haskell is a fantastic tool for thought. Its seeming lack
of "modern" features is not a flaw in the language; indeed, their
absence can help us see design details that are often obscured by more
conventional programming languages.

Haskell is considered a difficult language to learn, which is certainly true if
you come from traditional procedural languages. But rest assured, this
is not a *Haskell book.* You won't need more than a passing understanding of the
language's syntax and a few high-level idioms. Everything you'll need to
know will be shown later in this section. The ideas present in these pages are adaptable to any piece of technology you'd like, though they might require
strictly more discipline to maintain than is necessary in Haskell.

Because Haskell doesn't have mutable variables, programmers *simply can't* do
many of the quick hacks available in most languages --- the ones that solve
today's problem but often introduce a headache for a maintenance programmer
down the line. Think of every time you've added some functionality in a place it
didn't belong, simply because it was a convenient place, and you didn't have
time to do the proper refactoring. Technical debt like this accumulates over the
long run. Refactoring is always going to be necessary, and projects written in
languages that make refactoring difficult will necessarily slow
their development pace to cope. Haskell doesn't prevent anyone from writing
smelly code, but it makes it very apparent when something is added somewhere it
shouldn't be, as often such a change requires modifying code in *the entire call
stack.*

Haskell eschews traditional ideas of variables, mutability, unregulated
side-effects, object-oriented programming, and control flow. Tasks that require
significant amounts of "plumbing" in other languages are usually cogent and to
the point in Haskell. This is perhaps best demonstrated with an example. Let's
look at an implementation of quicksort in Python, taken from
@kutsurua_quicksort_2014.

```python
def partition(array, begin, end):
  pivot = begin
  for i in xrange(begin+1, end+1):
    if array[i] <= array[begin]:
      pivot += 1
      array[i], array[pivot] = array[pivot], array[i]
  array[pivot], array[begin] = array[begin], array[pivot]
  return pivot

def quicksort(array, begin=0, end=None):
  if end is None:
    end = len(array) - 1
  def innersort(array, begin, end):
    if begin >= end:
      return
    pivot = partition(array, begin, end)
    innersort(array, begin, pivot-1)
    innersort(array, pivot+1, end)
  return innersort(array, begin, end)
```

Without the helpful variable names, it would be tough to determine what this
function does. Compare this instead to an implementation of quicksort in
Haskell:

```haskell
quicksort :: Ord a => [a] -> [a]  -- ! 1
quicksort []     = []  -- ! 2
quicksort (x:xs) =  -- ! 3
  let smaller = quicksort (filter (<= x) xs)  -- ! 4
      bigger  = quicksort (filter (> x)  xs)
  in smaller ++ [x] ++ bigger  -- ! 5
```

Purists will argue that this is not a "true" implementation of quicksort, but
their arguments are too subtle to dwell on. In this one function, we can see
several examples of what makes Haskell an outstanding choice for straightforward reasoning about
software. Let's walk through the salient features, one line at a time.

Line [1](Ann) gives the *type signature* of our function `quicksort`. The function's name comes before the `::` token, and the type comes afterward. In
Haskell, a function's type is often its most interesting feature, so we will
return to types in @sec:types where we will have more space to dedicate to
it.  Suffice it to say, the type of `quicksort` is described in English as "a
function that takes a list container and returns a list container. This
operation is defined for any type that has a comparison operator."

On line [2](Ann), we see a definition for `quicksort`. There is no `function`
keyword, we simply dive in by giving the name of the function as the first
token, and then any arguments it takes before the equals sign, separated by
spaces. The `[]` symbol is special syntax for "an empty list," and so line
[2](Ann) says that the result of sorting an empty list is itself an empty list.

Line [3](Ann) gives *another* definition for `quicksort` --- this time for lists
that are not empty. It's common in Haskell to provide many separate definitions
for a function, each of which deals with special-case logic for particular
inputs.  Again `(x:xs)` is special syntax, that binds two (immutable) variables,
one called `x`, which is the "head" (or first element) of the list, and `xs`,
which contains the rest of the list.

On line [4](Ann), we recursively call `quicksort`, giving it an argument in
which we have kept only the elements that are less than or equal to `x`. The
result of this is bound to an immutable variable called `smaller`; this action
is equivalent to partitioning the array and taking all of the smaller elements.
The expression `filter (<= x) xs` here is probably foreign-looking, and
illustrates another feature of Haskell: we don't use parentheses to delimit
arguments in function calls. Instead, we just use spaces.

In Python, this expression would be `filter(lambda a: a <= x, xs)`. The Haskell
syntax `(<= x)` is shorthand for "give me an anonymous function that determines
if its input is less than or equal to `x`." We know this to be true because `<=`
is an operator that takes two arguments, but we've given it only one here, which
creates a lambda function.

Finally, on line [5](Ann), we concatenate the sorted list of elements smaller
than our pivot, our pivot after it, and the sorted list of larger elements.
The result is a sorted list.

Notice how little boilerplate was necessary to write this function. We didn't
have to write a single `for` loop, preferring recursion instead. Nor did we
require any mutation! Thinking in Haskell idioms can be tricky at first, but as
in the case of `quicksort`, usually results in code that is so short that it's
easy to convince yourself immediately of its correctness. We don't need to run a
complicated state machine on our brains to evaluate it; instead, we can reason in
smaller chunks, relying on the lack of mutation to ensure no "funny business" is
going on behind the scenes.

This book is entirely about writing code that doesn't do any funny business.
It's about designing abstractions that *can't possibly leak,* which means that
if you understand all of the individual pieces, you can put them together in any
way you'd like and *never be surprised.* Of course, you can design code like
this in languages other than Haskell too, but many of our guarantees
come from the fact that we have solemnly sworn not to mutate any global
variables. In a more traditional language, you will need to cross your heart
to maintain this invariant and keep constant vigilance against any
attempts to break it.


#### Reading Haskell

The Haskell code presented in this book is straightforward if for no reason other
than *this book is about designing software, not implementing it.* There simply
aren't many opportunities to write complicated code! For the most part, we will
satisfy ourselves with our operations' type signatures and of laws
interrelating these operations.

That being said, there are a few essential Haskell idioms you will need to
be familiar with to progress through these pages. As we saw in the previous section, the most important is that functions are called using spaces to
separate their arguments. A function call in Python notation:

```python
foo(a, bar(a, b, c), baz(d))
```

is expressed in Haskell like this:

```haskell
foo a (bar a b c) (baz d)
```

Notice that we introduce parentheses on the Haskell side whenever we'd like to
pass the *result of a function call* as an argument to another function call. If
we were instead to drop the parentheses like this:

```haskell
foo a bar a b c baz d
```

the equivalent Python code would be:

```python
foo(a, bar, a, b, c, baz, d)
```

which is very different indeed!

Haskell's grammar doesn't have statements, and so everything inside of a
function definition must be an expression. As you might expect, because of this,
individual expressions can get quite long. It's not uncommon to see code that
looks like this:

```haskell
mappend
  (strokeLocTrail spline)
  (mconcat
    (map
      (place spot)
      (pts ++ more_pts)))
```

Rather than fully embracing the surplus of parentheses like LISP programmers,
Haskellers will often use the dollar-sign operator to replace a pair of
parentheses. In almost all cases, the above would be instead be written as:

```haskell
mappend (strokeLocTrail spline)
  $ mconcat
  $ map (place spot)
  $ pts ++ more_pts
```

The idea of the dollar-sign operator is that it can be used to eliminate a pair
of parentheses that span until the end of the expression. That is to say, it can
remove a couple of parentheses that wrap the last argument to a function call. It
won't eliminate all parentheses: for example, It can't be used to eliminate the
parentheses around `(strokeLocTrail spline)` or `(place spot)` --- because these
don't span to the end of the expression. But it often works, and it's not
uncommon for function authors to reorder their "most interesting" argument to
the end, to allow for this dollar-sign idiom to be used. When we would like to
discuss the dollar-sign operator in the text (or refer to the function itself in
code), we wrap it in parentheses like `($)`. This is true of every symbolic
operator in Haskell.

Closely related to `($)` is `(.)`, which glues two functions together. Perhaps
you recall from grade-school math class, in which you were asked to rewrite
$f(g(x))$ as $(f\circ g)(x)$: the *composition of $f$ and $g$.*
The `(.)` operator in Haskell is reminiscent of this mathematical composition
operator, and is used in the same way. To illustrate, consider the silly
function:

```haskell
notTwice :: Bool -> Bool
notTwice b = not (not b)
```

We can rewrite this in function composition form:

```haskell
notTwice :: Bool -> Bool
notTwice b = (not . not) b
```

Such a transformation seems silly, but it can be combined with the fact that in
Haskell, the equals sign acts like a mathematical equals sign, which is to say,
just like in grade-school algebra, *we can cancel across it.* Because `b`
appears only at the end of both `notTwice b` and `(not . not) b`, they cancel
out, and we can rewrite `notTwice` more idiomatically as:

```haskell
notTwice :: Bool -> Bool
notTwice = not . not
```

As another silly example, the function:

```haskell
andNotOr :: Bool -> Bool
andNotOr b = and True (not (or False b))
```

can be rewritten in compositional form:

```haskell
andNotOr :: Bool -> Bool
andNotOr = and True . not . or False
```

This transformation is purely syntactic but represents a subtle cognitive shift
--- one of identities.  Given the definition `notTwice b = not (not b)`, we
might say "`notTwice` is a function that returns the result of calling `not` on
its argument twice." Given `notTwice = not . not`, we think instead "`notTwice`
*is* `not` after `not`."

In the latest definition  of `andNotOr`, we have two expressions --- `and True`
and `or False`, which are arity-2 functions being given a single argument.
What's up with this? In Haskell, all functions are *curried,* meaning you can
give arguments to them one at a time. The result is that we can *partially fill
in parameters* to specialize functions for a particular behavior. Maybe
we want to give a function that checks if two strings are equal:

```haskell
eqString :: String -> String -> Bool
eqString a b = a == b
```

We can use `eqString` to define another function that checks if a given string
is `"Algebra"`:

```haskell
isAlgebra :: String -> Bool
isAlgebra b = eqString "Algebra" b
```

which can then be "canceled" down to:

```haskell
isAlgebra :: String -> Bool
isAlgebra = eqString "Algebra"
```

Because we don't need to fill all of the function's arguments at once, we can
easily define other terms which fix some arguments, while leaving others to be
filled in. This operation's ease encourages Haskell code to be written at
an extreme level of abstraction and then specialized down again to the desired
functionality. For example, consider the common task of adding up every number
in a container. If you're lucky, your container library might contain such a
method, but many people would just write a `for` loop, which likely takes less
time to implement than searching for the right function. In Haskell, this
operation is called `foldMap`, and has the following type signature:

```haskell
foldMap
    :: ( Foldable t  -- ! 1
       , Monoid m  -- ! 2
       )
    => (a -> m)  -- ! 3
    -> t a
    -> m
```

This function is obnoxiously general; it works for any container type `t` (the
constraint `Foldable t` at [1](Ann) ensures we know how to iterate over that
type), and for any monoidal ([2](Ann)) query we could ask for over that
container (see @sec:monoid for more information on monoids.) This function then
takes a transformation (given by [3](Ann) from the type inside the container to
the desired value, the container itself, and spits out the result. This one
function that we wrote to sum values can also be used to flatten nested
lists, construct interactive "wizard" configuration programs, find containing
spans over a collection of time intervals, and so much more. It's amazing
that it can do all of these things over lists, arrays, hash maps, rose trees,
sets, and any other data structure imaginable. All of this in one little
function.

Perhaps more interestingly is that when all of your building blocks are
unimaginably general, it takes no work to write new code as powerful as
`foldMap`. Usually, I'll write the code that solves the problem I have in mind,
and then ask Haskell to give me the type of the thing I just wrote. The function
I've just written is usually more general than what I had in mind. With a
glimpse of what's possible, it's only a few more keystrokes to generalize what's
left. Haskell code seems to mostly write itself, requiring me only as the
automaton responsible for writing it down.

We are brought now to the final Haskell idiom you will need for this
book. It's the function `id`, pronounced "identity", which simply returns its
argument. The definition of `id` is not very interesting:

```haskell
id :: a -> a
id a = a
```

The `id` function does nothing; we can insert it anywhere
(syntactically valid) we want in an expression without changing the result. So
what use is `id` then? Consider the analogous number 0, which can be added
anywhere without changing the answer! Zero does nothing, and corresponds to
nothing! So what good is it? Zero is used as a placeholder, or as a solution to
an equation that you expect to balance out. Our `id` function serves a similar role.
We will primarily use it as a syntactic placeholder when we need to give an
argument but want to give one that doesn't change the result.

We will also use `id` to state that two functions "cancel" one another. Consider
two functions:

```haskell
plusTwo :: Int -> Int
plusTwo x = x + 2

minusTwo :: Int -> Int
minusTwo x = x - 2
```

It's clear that `plusTwo` and `minusTwo` are inverses of one another; doing
either after the other is equivalent to doing nothing at all. We can formalize
this (described in more detail in @sec:laws) by declaring the following two
equations hold:

```{.haskell law="plusTwo/minusTwo"}
plusTwo . minusTwo = id
```

```{.haskell law="minusTwo/plusTwo"}
minusTwo . plusTwo = id
```

This wraps up the Haskell idioms you will need to read
Algebra-Driven Design. As a reminder, they are:

* Functions are called without parentheses and commas like in most languages.
    Instead, we separate arguments using spaces, and put parentheses around
    expressions which require spaces in their own right.
* We can replace a pair of parentheses that go until the very end of an
    expression using the `($)` operator.
* We can glue two or more functions called one after another by using the `(.)`
    operator. This is often useful for getting the expression into a form where
    we can "cancel" a variable on either side of the equals sign.
* The `id` function is the "do nothing" function, which simply returns the
    given argument. It is often used to show that the result of composing
    functions together is annihilation, that is to say, that they are inverses
    of one another.

Familiarity with Haskell's standard library will be assumed throughout this
text. Often the behavior of a function will be clear from context, but if you
find yourself confused about any function, searching for it on Hoogle[^hoogle]
will bring up its documentation and examples of its use.

[^hoogle]: Hoogle can be found at
  [https://www.stackage.org/](https://www.stackage.org/). The search bar at the
  top of the page is what you're looking for. In addition to functions, it can
  also give you information on unfamiliar types!


#### Understanding Haskell Types {#sec:types}

The most intriguing thing about Haskell is its type system, which is capable of
expressing (and subsequently checking at compile time) business-logic
invariants. While we will not be doing anything so advanced here, it's essential
to understand Haskell's type system's basics. In a land where there are no
side-effects like getting the current time or making an API call, and where
mutation is disallowed, the type of a function is extremely informative about
what that function is, and, more importantly, what it can do.

Haskell represents functions using the single-arrow symbol. The type of `not`,
which inverts a boolean value, is:

```haskell
not :: Bool -> Bool
```

The `Bool` on the left side of this arrow is the type of the function parameter,
and the `Bool` on the right is the return type of the function itself. As
another example, consider a hypothetical function which converts its integer
argument into a string:

```haskell
toString :: Int -> String
```

Of course, functions can take multiple arguments; but the syntax isn't anything
you'd expect it to be. For every argument, we introduce another arrow. The
function that adds three integers together, and then calls `toString` on them
has this type:

```haskell
addThreeThenToString :: Int -> Int -> Int -> String
```

We read this as a function that returns a string. Each arrow before that delimits a parameter. To make things more
challenging, functions can take parameters that are themselves functions, and
these "function types" need to show up in the type signature. As an
illustration, there are lots of possible ways of turning an integer into a
string --- we might want to display `3` as any of `"3"`, `"three"`,
`"THREE!!!"`, `"iii"`, or anything else. We can parameterize our "add
three" function by how it should produce a string, rather than hard-coding it to
call `toString`.

```haskell
addThreeThenToString
    :: (Int -> String)
    -> Int
    -> Int
    -> Int
    -> String
```

Our `addThreeThenToString` function now takes *four* arguments, the first being
a function that turns an integer into a string, and the other three being
integers. And at the end of the day, `addThreeThenToString` returns a string. Of
course, nothing is stopping us from writing functions that take functions that
take functions that take functions... nothing other than our mental health, at
least. Fortunately, nothing gets that complicated here.

The other feature to be aware of in Haskell's type system are its *type
variables.* Type variables are comparable to generics in C# and Java, and to
templates in C++. Common, concrete types in Haskell all begin with an uppercase
letter (c.f. `Bool`, `Int` and `String`), while type variables begin with a
lowercase letter. For example, imagine we'd like to write a function that finds
the length of a list. Clearly, such a function doesn't depend on what sorts of
values are contained in the list; it only cares about the list structure itself.
We can encode this in Haskell with the type:

```haskell
length :: [t] -> Int
```

Here, the square brackets mean "a list of", and the `t` is our type variable. The
type variable `t` acts as a stand-in for *any type at all,* meaning we can call
`length` just as easily on a list of integers, booleans, or even more exotic
types such as lists of lists of functions. Anything can fill in the `t`!

We can take this idea further. Many languages have generics, but very few can
parameterize a generic type itself. Let's look again at `foldMap` from the
previous section:

```haskell
foldMap
    :: ( Foldable t
       , Monoid m
       )
    => (a -> m)
    -> t a
    -> m
```

Here, the type variable `t` doesn't correspond to a *type* per se, but instead a
container type that itself requires a type to be filled in before it can be
used. Mastering this is a subtle point. Illustrated less abstractly,
consider the `map` function, which runs a function over a list of numbers. In
its most primitive form, it might look like this:

```haskell
map :: (Int -> Int) -> [Int] -> [Int]
```

Such a function doesn't ever require the integer-ness of its arguments,
and so we can generalize it from working over integers to working over any type
`a`:

```haskell
map :: (a -> a) -> [a] -> [a]
```

This is still entirely in the realm of traditional generics that you know and love.
Nothing crazy yet! But here is where things are about to get wild. We realize that not only can we
implement `map` for lists, but also for arrays and hash maps! We want to be able
to abstract over the notion of which sort of container we're using --- but
whatever is inside of those containers should still change from `a` to `b`. In
the following definition of `map`, we have introduced a new type variable `f`
that at [1](Ann) and [2](Ann) takes a type as an argument!

```haskell
map
    :: (a -> b)
    -> f a  -- ! 1
    -> f b  -- ! 2
```

In this sense, types behave as functions; they can take arguments, which
are also filled in by juxtaposing types and separated by a space. Furthermore,
like functions, parameterized types need not be "fully filled in" before being
used.

The result of this abstraction can be perplexing for a new practitioner of
Haskell, but seems to be a price worth paying; by writing such remarkably general
code, we are rewarded with extreme code reuse, and exceptional
forwards-compatibility: the `foldMap` function was added to the standard library
sometime before 2009, and will be usable --- without any modifications or
explicit support -- for containers that do not yet exist.


#### Equational Laws {#sec:laws}

Throughout the main content of this book, you will often find blocks typeset
as following:

```{.haskell law="plus commute"}
∀ (x :: Int) (y :: Int).
  x + y = y + x
```

These blocks denote *algebraic equations,* and are also known as *algebraic
laws.* Unsurprisingly, given this book's theme, algebraic equations will
prove to be of the utmost importance. As such, we must understand
exactly what it is that's being said in these blocks.

Laws consist of three parts: the *binders*, the *left hand side* (LHS for
short,) and the *right hand side* (RHS). Binders are introduced with the
"forall" symbol `∀` and are terminated by a dot. As you might expect, the LHS
occurs on the left side of the equality, and the RHS on the right.
Syntactically, laws follow this template:

```{.haskell law="lhs rhs"}
∀ binders. left hand side = right hand side
```

The meaning of equality in these equations is subtle and not very familiar to
programmers. This `=` symbol doesn't mean assignment, an equality test, nor does
it introduce a definition. Instead, it is considered *equality* in the
mathematical sense; that is, that whatever is on the LHS is *exactly the same*
as whatever is on the RHS.

An underlying principle of Algebra-Driven Design is that syntax exists to allow users to communicate their ideas to the computer. But this is all
that syntax does; we will learn how to separate completely our public interface
from its underlying implementation. This means that, from the
library's perspective, two different syntactic constructions can describe identical values.
Laws are how we can specify exactly when these syntactic constructions should
align in their meaning. To see this more clearly, let's analyze our earlier
example. Here it is again:

```{.haskell law="plus commute"}
∀ (x :: Int) (y :: Int).
  x + y = y + x
```

This law describes the *commutativity* of addition, which is to say, that the
order in which you add two numbers doesn't matter. Five plus two is seven, just
as two plus five is also seven. The equation above introduces two integer
variables (in the mathematics sense), `x` and `y`, and states that, no matter
what numbers you pick for `x` and `y`, it is always going to be the case that `x
+ y` is equal to `y + x`. Clear as crystal.

Laws are best considered inviolable laws of nature. They are not equality tests,
because the word "test" suggests the possibility of failure. Our algebraic
equalities can't fail to hold; they simply are. Of course, it's possible to
give an implementation in which the laws do not hold --- but this is just
as indicative of incorrect behavior as a math library, which says that four plus
eleven is two. This mindset encourages us to *specify* essential properties of
the systems we're designing as equational laws; by doing so, we acquire a
convenient, formal language for nailing down requirements.

For readers coming from the ML-family of programming languages, they might be
surprised to learn that the LHS of a law need not be a *pattern.* The left-hand
side of an equation can be any syntactically valid form; it doesn't need to be
something on which you can pattern match. For example, we can write the
following law:

```{.haskell law="filter false"}
∀ (xs :: [a]).
  filter (const False) xs = []
```

which states that filtering a list with a predicate that never returns true will
result in an empty list. This isn't a legal definition in ML languages like
Haskell, where `const False` is an expression but not a data constructor, and
thus can appear only on the right-hand side of a definition. Such a restriction
is relaxed when we are looking at algebraic equalities.

Laws are allowed to use a variable *non-linearly,* which is to say, more than
once on a given side:

```{.haskell law="times two"}
∀ (x :: Int).
  x + x = 2 * x
```

This pattern is commonly used to specify how multiple operations interrelate
with one another. For example, we can use it to describe the idea that "you get
back what you put in to a map." By looking up the same key we just inserted, we
can be sure we'll get back our original value:

```{.haskell law="lookup insert"}
∀ (m :: Map key val) (k :: key) (v :: val).
  lookup k (insert k v m) = Just v
```

On the other hand, specifying that we shouldn't get back values we didn't put in
is trickier. The trick requires a little mathematical deftness to decompose the
problem into two cases. In the first case, we attempt to look up a value
in an empty container, which should always fail:

```{.haskell law="lookup empty"}
∀ (k :: key).
  lookup k empty = Nothing
```

But to show that a key doesn't exist in a non-empty list is a little harder. In
cases like these, we can use *conditional* equations --- ones which are true
sometimes, but not always applicable. We introduce conditional clauses by way of
a boolean predicate and a `=>` symbol before the LHS:

```{.haskell law="lookup inductive"}
∀ (m :: Map key val) (k :: key) (k' :: key) (v :: val).
  k /= k' =>
    lookup k (insert k' v m) = lookup k m
```

In this law, we first check to see if `k` is different than `k'`, and say that
this equation holds only if they don't correspond to the same value!
Remember --- we can ensure two terms are identical by using the same variable
for both, but nothing says that two different variables can't be instantiated
with the same value! If the key we're looking for is different from the key inserted, then this is not the value we're looking for, so the result is
the same as looking for our key in the remainder of the `Map`.

While the form of these last three laws suggests an implementation of maps
that involves linked lists, there is no need for this to be the case.
All we are asserting is that the visible behavior of the `Map`'s lookup policy
is indistinguishable from a naive linked list implementation.

