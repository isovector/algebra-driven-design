## Glossary {-}

::: {#glossary}

`(<>)`

: a symbolic alias for `mappend`.

`(<$>)`

: a symbolic alias for `fmap`.

`(<*>)`

: the applicative functor "apply" operation --- runs a structure full of
  functions over a structure full of values. See @sec:applicative. `(<*>) ::
  Applicative f => f (a -> b) -> f a -> f b`

`(=<<)`

: like `(>>=)` but with its arguments flipped.

`(>>=)`

: the monadic "bind" operation --- replaces the leaf of a structure with a
  structure generated from the value that used to be there. `(>>=) :: Monad m =>
  m a -> (a -> b) -> m b`

algebra

: a closed set of types, constructors of those types, and equations governing
  terms of those types. An algebra is characterized by its composability, and
  that every constructable term is meaningful.

algebraic structure

: a reusable algebra; one which occurs regularly.

annihilation

: a generalization of the law $x \times 0 = 0$. See @sec:annihilative.

applicative functor

: a functor equipped with `pure` and `(<*>)` operations, allowing for lifted
  function application. See @sec:applicative.

arbitrary

: the canonical name of a generator in QuickCheck.

associativity

: an algebraic property which states that all possible parenthesizations of an
  expression are equivalent.

closure

: the property of a mathematical system that states every possible combination
  of inputs is meaningful and well-defined.

commutativity

: an algebraic property which states that the order of arguments can be swapped
  without changing the meaning.

composition

: the act of building something out of two smaller pieces. Might refer to
  function composition or algebraic composition.

constructor (algebraic)

: a means of introducing a term of a type in an algebra. Constructors are
  divided into two categories: terminal and inductive.

constructor (data)

: a language feature in Haskell used for creating values of types

constructor (inductive)

: a constructor which takes an argument of the same type as its output.
  Inductive constructors "decorate" terminal constructors and other inductive
  constructors.

constructor (terminal)

: a constructor which requires no terms of the same type as its output.

continuation-passing style (CPS)

: a functional programming technique in which program flow is explicitly laid
  out as function calls. CPS can be used to eliminate intermediate data
  structures and for more explicit control flow. See @sec:cps.

denotation

: the conceptual implementation of an algebra. A denotation conveys exactly the
  right mental model for how an algebra works, regardless of whether or not
  that's how it is actually implemented.

denotative

: a property of systems stating that they are made up of sub-expression
  structure, and that the meaning of an expression depends only on the meaning
  of its sub-expressions.

distinguished element

: a term of particular interest in an algebra, often because it is involved in
  an equation.

distributivity

: the ability to pull one function call out of another without changing the
  meaning of the expression.

equation

: a statement of equality between two (possibly quantified) terms. Equality is
  always defined with respect to some (or all) observations.

fmap

: the characteristic operation of a functor. Maps a function point-wise over
  every element "contained" in the functor. `fmap :: Functor f => (a -> b) -> f
  a -> f b`

free

: an instance of an algebraic structure which satisfies no equations other than
  those strictly necessary. Free structures are particularly good choices when
  instantiating type variables for property-based testing.

functor

: generalization of a container data structure. All functors are parameterized
  over some "contained" type, and support mapping functions over those values of
  that type. See @sec:functor.

generator

: a piece of code capable of generating random values of a particular type. Used
  widely in property-based testing.

group

: an algebraic structure which is associative, invertible, and has an identity
  element. See @sec:group.

idempotence

: a property stating that performing an action twice is equivalent to doing it
  only once. See @sec:idempotence.

identity

: a distinguished element which doesn't affect the result of a computation when
  used as an argument.

initial encoding

: an implementation of an algebra which encodes every constructor as a primitive
  form, using pattern matching to force laws to hold. Initial encodings are
  often easy to get right and hard to make fast.

homomorphism

: a transformation which preserves algebraic structure.

law

: see "equation"

linear

: the property of a variable that it is used only once

mappend

: canonical name in Haskell for the semigroup operation. `mappend :: Semigroup a => a -> a -> a`

mempty

: canonical name in Haskell for the monoid identity. `mempty :: Monoid a => a`

monad

: the capability of a data structure to be able to sequence effectful
  operations.

monoid

: an algebraic structure equipped with associativity and an identity element.

morphism

: see "homomorphism"

observation

: a means of "extracting" information out of an algebra. Observations are what
  give semantics to algebras.

primitive form

: a data constructor, used to implement constructors of an algebra in an initial
  encoding.

property-basted testing

: a strategy for generating unit tests by ensuring that properties always hold
  on randomly generated data.

pure

: an applicative functor operation which creates container structure from a
  single value. `pure :: Applicative f => a -> f a`

QuickCheck

: a Haskell library which implements property-based testing. See
  @sec:quickcheck.

QuickSpec

: a Haskell library which uses signatures to discover laws and generate property
  tests for any software implementation. See @sec:quickspec.

semantics

: the meaning of an algebra.

semigroup

: an algebraic structure characterized by associativity. See @sec:semigroup.

semilattice

: an algebraic structure with associativity, commutativity, and idempotence. See
  @sec:semilattice.

signature

: a manifest of the constructors and terms of an algebra for use with QuickSpec.

ST monad

: a Haskell data structure for introducing local, referentially-transparent
  mutable variables.

term

: an expression made from constructors of an algebra.

test size

: in a generator, corresponds roughly to the desired complexity of the resulting
  random value.

:::

