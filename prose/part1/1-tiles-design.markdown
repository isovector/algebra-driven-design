## Tiles {#sec:tiles}

Doing Algebra-Driven Design requires a tremendous shift in perspective; we want to
sculpt our software in thought, having it mostly worked out before we ever write
a line of code. The reason behind this is that cajoling computers into executing
our ideas takes effort, and empirically, programmers are likely to continue
sinking costs on a lousy implementation, rather than starting again once they've
figured out what they're building. Working out designs "in code" is akin to
taking on technical debt before you've even started.

Instead, we should reason through our design's building blocks, fully
working out their properties and interrelationships. We will hold off on writing
any code for quite some time, which can be disconcerting; there will be nothing
to "get our hands on" and interact with. We can't "get it running, then get it
right after." Thus, all of the playing must happen in our minds, which is not a
fair thing to ask of beginning practitioners. So we will start slowly, and use
an incredibly visual first example. Hopefully, this will help convince you that it
is possible to understand everything about a library before even a
single line of code is written to implement it.

Our example algebra follows closely from @henderson_functional_2002, and is one
for constructing images out of recursively-subdivided images, any of which
might be modified by a simple spatial transformation. For example, our algebra
can describe images like @fig:ex1, @fig:ex2 and @fig:ex3.

```{#fig:ex1 design=code/Tiles/Efficient.hs label="Example 1"}
nona (cols [haskell, behind haskell (flipH haskell), flipH haskell]) (swirl $ swirl $ cw haskell) haskell
```

```{#fig:ex2 design=code/Tiles/Efficient.hs label="Swirling mathematicians"}
swirl $ cw church
```

```{#fig:ex3 design=code/Tiles/Efficient.hs label="Sierpinski Carpet"}
_carpet 3 59
```

The eventual algebra we will find is small and powerful --- with only modest
additions, @henderson_functional_2002 uses it to recreate some of M. C. Escher's
artwork.


### Basic Building Blocks

We begin by noting that there exists a type for tiles, which appropriately
we will name `Tile`:

```haskell
data Tile
```

We can illustrate our tile-algebra with two *terminal* constructors: `haskell`
and `church`. Our final implementation will involve means for a user to get
custom images into the system. Still, for now, we choose not to worry ourselves
about image formats or input/output and simply provide these two tiles by fiat.

```haskell
haskell :: Tile
church :: Tile
```

When rendered, these tiles look like @fig:haskell and @fig:church.

```{design=code/Tiles/Efficient.hs}
haskell
```

```{design=code/Tiles/Efficient.hs}
church
```

All terms in an algebra are built from terminal constructors (like `haskell` and `church`
above) and *inductive* constructors: ones which "derive" new terms based on
existing terms. For example, if we have a `Tile` we might want to rotate it 90
degrees clockwise, as shown in @fig:cw_haskell.

```{design=code/Tiles/Efficient.hs}
cw haskell
```

Let's call this operation `cw` ("clockwise"). Because `cw` is an operation that
consumes a `Tile` and produces a new one, its type is:

```haskell
cw :: Tile -> Tile
```

Because `cw` works on *any* `Tile`, we can apply it to the result of itself as
in @fig:cw_cw_haskell_.

```{design=code/Tiles/Efficient.hs}
cw (cw haskell)
```

However, an interesting thing happens when we call `cw` on itself *four* times
--- we somehow get back to where we started. If you don't believe me, compare
@fig:haskell and @fig:cw_cw_cw_cw_haskell_! Because we all have decades' worth of
experience in the domain of geometry, this result isn't particularly startling
--- which is always the danger when analyzing a simple structure. But when you
stop to think about it, this property of "four times around gets you back where
you started" is quite the *defining characteristic* of rotation by 90 degrees.
If the number of times we needed to call `cw` to get back to the original `Tile`
were not four but instead $n$, our rotation *must instead* be by $\frac{360}{n}$
degrees. Thus, we can closely specify what it is we're talking about by
requiring the following law always holds true:

```{design=code/Tiles/Efficient.hs}
cw (cw (cw (cw haskell)))
```

```{.haskell law="cw/cw/cw/cw"}
∀ (t :: Tile).
  cw (cw (cw (cw t))) = t
```

It's important to understand where this law comes from. Because of our problem's underlying geometry, this law is *foisted upon us;* we have
no choice but to accept it. Our understanding of the problem
itself has uncovered this law, and there is nothing we can do about it,
short of changing our minds about what `cw` should do. This is always the case
when designing algebras; because an algebra must have consistent semantics,
decisions we make ripple throughout the design, forcing constraints upon us. Our
final design will consist of dozens of equations like these. *Any implementation
of the design is necessarily a solution to that system of equations.*

As you work through this or any other algebra, keep in mind the slogan,

> **It's the equations that really matter!**

The laws are of critical importance, as they are what foist meaning upon our
otherwise empty syntactic constructs.

Let's return to our algebra. Of course, there is no reason to privilege one
direction of rotation over another. Let's also provide a `ccw`
("counterclockwise") constructor with the same type, as illustrated in
@fig:ccw_haskell.

```{design=code/Tiles/Efficient.hs}
ccw haskell
```

We don't require both `cw` and `ccw` --- having both gives us no
additional expressiveness than having only one. To see this for yourself, note
that rotating counterclockwise once is equivalent to rotating clockwise three
times, and vice versa. If we valued extreme parsimony, we could certainly get by
having only one of these combinators, but we will provide both because they are both useful. However, having two ways of getting the same result gives us twice
as many chances to write a bug. But we can subvert such a problem by simply
giving a law relating `cw` to `ccw`; so long as the law holds, there can be
no bug here.

The most obvious relationship between `cw` and `ccw` is that they are inverses
of one another. Performing either after the other is equivalent to doing
nothing.

```{.haskell law="ccw/cw"}
∀ (t :: Tile).
  ccw (cw t) = t
```

```{.haskell law="cw/ccw"}
∀ (t :: Tile).
  cw (ccw t) = t
```

Again, these laws are nothing we have control over; they are required to hold
for any `cw` and `ccw` that could correspond to our informal
notions that these operations should rotate their arguments 90 degrees. In time,
you will learn to analyze software in terms of which laws it satisfies, that is
to say, you will be able to visualize software via its equations.

Our equations don't just look like the sort of algebra you did in grade-school;
indeed, we can use them in just the same way. For example, we can derive the
fact that rotating clockwise three times is equivalent to going counterclockwise
once, via simple algebraic manipulation. We start with the fact that rotating
clockwise four times is equivalent to doing nothing, then use `ccw` on both
sides of the equation, and then use the fact that `ccw` eliminates a `cw`:

```{.haskell .proof}
  ccw t
= -- .via cw/cw/cw/cw
  ccw (cw (cw (cw (cw t))))
= -- .via ccw/cw
  cw (cw (cw t))
```

This sort of algebraic manipulation is extraordinarily useful when it comes to
solving your algebra's system of equations (that is to say: actually
implementing it.)

So far, our algebra isn't very powerful. Let's introduce some new constructors.
We will add the capability to mirror a tile horizontally --- illustrated by
@fig:flipH_haskell.

```haskell
flipH :: Tile -> Tile
```

```{design=code/Tiles/Efficient.hs}
flipH haskell
```

Mirroring a tile is a fundamentally new idea in our algebra; it simply can't be
expressed in terms of `cw` or `ccw`. Its major governing law is that `flipH` is
its own inverse:

```{.haskell law="flipH/flipH"}
∀ (t :: Tile).
  flipH (flipH t) = t
```

However, we note that mirroring a tile, rotating it twice, and then mirroring it
back is equivalent to just turning it twice. This equation helpfully relates
`flipH` to `cw`, entangling their semantics. Again, this law is nothing other
than a logical conclusion if `flipH` flips about its X-axis and `cw . cw`
simultaneously flips its X- and Y-axes. This is a fact about our problem and is
the only reasonable conclusion if `flipH` and `cw` behave like our minds' eyes
say they do.

```{.haskell law="flipH/cw/cw/flipH"}
∀ (t :: Tile).
  flipH (cw (cw (flipH t)) = cw (cw t)
```


Exercise

:  Prove `flipH . cw^{2*n} . flipH = cw^{2*n}`, where the `^` operation means
   repeated composition. For example, `cw^4 = cw . cw . cw . cw`.


A little mental geometry shows us that horizontally flipping a clockwise
rotation is equivalent to rotating counterclockwise a horizontal flip. This is a
delightful law that relates `cw` to `ccw` under the `flipH` transformation:
to say, that

```{.haskell law="x-symmetry"}
∀ (t :: Tile).
   flipH (cw t) = ccw (flipH t)
```

Exercise

:  Find a way of recreating @fig:flipV_exercise, using only `cw`, `ccw` and
   `flipH`.


```{#fig:flipV_exercise design=code/Tiles/Efficient.hs label="Recreate this tile"}
ccw (flipH (cw haskell))
```

The operation carried out in @fig:flipV_exercise is equivalent to flipping a
`Tile` vertically. Rather than require our users to perform the complex set
of operations to create it by hand, we will offer this effect as a constructor
in its own right.

```haskell
flipV :: Tile -> Tile
```

Of course, `flipV` is also its own inverse, but two other interesting equations
as well -- that we can derive it from `cw`, `ccw` and `flipH`, and that
performing both flips is equivalent to doing two rotations.

```{.haskell law="flipV/flipV"}
∀ (t :: Tile).
  flipV (flipV t) = t
```

```{.haskell law="ccw/flipH/cw"}
∀ (t :: Tile).
  flipV t = ccw (flipH (cw t))
```

```{.haskell law="flipV/flipH"}
∀ (t :: Tile).
  flipV (flipH t) = cw (cw t)
```


Exercise

:  Derive the fact that `flipV` is its own inverse, using any of the *other*
   laws we've given for our algebra.


Solution

:   ```{.haskell .proof}
      flipV (flipV t)
    = -- .via flipV
      flipV (ccw (flipH (cw t)))
    = -- .via flipV
      ccw (flipH (cw (ccw (flipH (cw t)))))
    = -- .via cw/ccw
      ccw (flipH (flipH (cw t)))
    = -- .via flipH/flipH
      ccw (cw t)
    = -- .via ccw/cw
      t
    ```


Exercise

:  Derive a proof that `flipV . flipH = cw . cw`

Solution

:   ```{.haskell .proof}
      flipV (flipH t)
    = -- .via flipV
      ccw (flipH (cw (flipH t)))
    = -- .via ccw
      cw (cw (cw (flipH (cw (flipH t)))))
    = -- .via x-symmetry
      cw (cw (flipH (ccw (cw (flipH t)))))
    = -- .via ccw/cw
      cw (cw (flipH (flipH t)))
    = -- .via flipH/flipH
      cw (cw t)
    ```


### Subdividing Space

It's time to add some intrigue to our algebra. As great as transforming square
tiles is, it's just not enough to capture our imaginations for long. We will now
introduce our killer feature: being able to compose multiple tiles together. The
most exciting of these is `beside`, which lays out one tile beside another.

Because our tiles are always square, we need to determine how to *close under*
this operation; recall, every operation in an algebra must take valid inputs to
valid outputs. Simply sticking one square tile beside another would result in a
rectangular image, which would not be a square tile! Instead, we decide that to maintain closure, we will first subdivide our square into two
rectangular halves, and then fill each half, stretching the tiles to cover the
space. Our new operation is illustrated in @fig:beside_church_haskell.

```haskell
beside :: Tile -> Tile -> Tile
```

```{design=code/Tiles/Efficient.hs}
beside church haskell
```

Of course --- because of the closure property, we can freely nest calls to
`beside`, as in @fig:beside_haskell_beside_haskell_haskell_ and
@fig:beside_beside_haskell_haskell_beside_haskell_haskell_.

```{design=code/Tiles/Efficient.hs}
beside haskell (beside haskell haskell)
```

```{design=code/Tiles/Efficient.hs}
beside (beside haskell haskell) (beside haskell haskell)
```

As you might expect, we should look for some laws relating `beside` to our
other constructors. This should always be our modus operandi when working with
algebras; for every new constructor you add, look for a way to connect it to
other things in your algebra. Over time, this web of connections will
strengthen and often help us find properties that are too hard to deduce
by intuition alone. In this case, because `beside` is aligned along the X-axis,
we should search for equalities that preserve the X-axis. Interestingly, some
mental manipulation shows us that `flipH` distributes through `beside`, but in
doing so, flips the order of its arguments:

```{.haskell law="flipH/beside"}
∀ (t1 :: Tile) (t2 :: Tile).
  flipH (beside t1 t2) = beside (flipH t2) (flipH t1)
```


Exercise

:  Prove `flipH (flipH (beside t1 t2)) = beside t1 t2` in two separate ways.


By some clever manipulation --- reminiscent of how we derived `flipV`, we can
position one tile above another --- as shown in @fig:above_exercise.


Exercise

: Recreate @fig:above_exercise, using `beside`, `cw` and `ccw`.


```{#fig:above_exercise design=code/Tiles/Efficient.hs label="Recreate this tile"}
ccw (beside (cw haskell) (cw church))
```

Again, rather than make our users jump through hoops, we will just provide
`above` as its own constructor.

```haskell
above :: Tile -> Tile -> Tile
```

```{#fig:quad design=code/Tiles/Efficient.hs}
above (beside (cw haskell) (cw (cw church))) (beside church (ccw haskell))
```

```{.haskell law="above"}
∀ (t1 :: Tile) (t2 :: Tile).
  above t1 t2 = cw (beside (ccw t1) (ccw t2))
```

Intuitively, we can also rewrite an `above` of `beside`s as a `beside` of
`above`s, so long as we swap the top-right and bottom-left tiles when we do so.

```{.haskell law="above/beside"}
∀ (a :: Tile) (b :: Tile) (c :: Tile) (d :: Tile).
  above (beside a b) (beside c d) =
    beside (above a c) (above b d)
```

The construction of four tiles in a square --- as in @fig:quad --- turns out to
be a particularly common pattern. Let's call it `quad`:

```haskell
quad :: Tile -> Tile -> Tile -> Tile -> Tile
```

```{design=code/Tiles/Efficient.hs}
quad haskell (flipH haskell) (flipV haskell) (flipV (flipH haskell))
```

```{.haskell law="quad"}
∀ (a :: Tile) (b :: Tile) (c :: Tile) (d :: Tile).
  above (beside a b) (beside c d) = quad a b c d
```

As an even more special case, we can rotate one tile as we move through a quad,
creating a sort of `swirl` effect as in @fig:swirl_above_church_haskell_. This
operation is given by:

```haskell
swirl :: Tile -> Tile
```

```{design=code/Tiles/Efficient.hs}
swirl (above church haskell)
```

and is subject to the law:

```{.haskell law="swirl"}
∀ (t :: Tile).
  quad t (cw t) (ccw t) (cw (cw t)) = swirl t
```

We now come to final spatial operation, `behind`, which allows us to layer
one tile on top of another as in @fig:behind_church_haskell:

```haskell
behind :: Tile -> Tile -> Tile
```

```{design=code/Tiles/Efficient.hs}
behind church haskell
```

As great as all of these spatial constructions are, it will be helpful to have
another terminal constructor --- one which fills the space with a given color.
Let's call it `color`, and give it the type:

```haskell
color
    :: Double  -- ^ red
    -> Double  -- ^ green
    -> Double  -- ^ blue
    -> Double  -- ^ alpha
    -> Tile
```

Each of these channels should be within the closed interval `[0,1]`. Of course,
nothing in the typesystem requires this to be the case, so we will need to
constrain it with a law:

```{.haskell law="clamp channels"}
∀ (r :: Double) (g :: Double) (b :: Double)
      (a :: Double).
  color r g b a =
    color (clamp 0 1 r)
          (clamp 0 1 g)
          (clamp 0 1 b)
          (clamp 0 1 a)
```

Figures @fig:color_orange, @fig:color_blue and @fig:color_blue_alpha give some
illustrations of the different channels, and how alpha blending works.


```{#fig:color_orange design=code/Tiles/Efficient.hs}
color 1 0.8 0 1
```

```{#fig:color_blue design=code/Tiles/Efficient.hs}
color 0 0.67 0.87 0.5
```

The `color` combinator has the interesting property that is unaffected by `cw`
and `flipH`:

```{.haskell law="cw/color"}
∀ (r :: Double) (g :: Double) (b :: Double)
      (a :: Double).
  cw (color r g b a) = color r g b a
```

```{.haskell law="flipH/color"}
∀ (r :: Double) (g :: Double) (b :: Double)
      (a :: Double).
  flipH (color r g b a) = color r g b a
```

The semantics of `color` are "obvious" to a human, but it's not entirely clear
how to *specify* such an operation. Of particular challenge is its interaction
with `behind` as demonstrated in @fig:color_blue_alpha, where we should expect
alpha compositing to occur. We can attempt a partial specification by noting
what happens with extreme alpha values. If the alpha channel of the `color` in
front is fully set, it doesn't matter what was behind it:

```{#fig:color_blue_alpha design=code/Tiles/Efficient.hs}
behind church (color 0 0.67 0.87 0.5)
```

```{.haskell law="opaque"}
∀ (t :: Tile) (r :: Double) (g :: Double) (b :: Double).
  behind t (color r g b 1) = color r g b 1
```

Likewise, if there is zero alpha, it's the same as having not invoked `behind`
at all.

```{.haskell law="transparent"}
∀ (t :: Tile) (r :: Double) (g :: Double) (b :: Double).
  behind t (color r g b 0) = t
```

This last law shows that `color r g b 0` is a right-identity for `behind`;
putting it in changes nothing. In some sense, this distinguishes `color r g b 0`
as an "object of interest," and as such, is worth including explicitly in our
algebra --- as illustrated in @fig:empty.

```haskell
empty :: Tile
```

```{.haskell law="empty"}
∀ (r :: Double) (g :: Double) (b :: Double).
  color r g b 0 = empty
```

```{#fig:empty design=code/Tiles/Efficient.hs label="empty --- intentionally left blank"}
empty
```


### Observations

Take a moment to appreciate just how much understanding we've gained about our
problem. We've reasoned through specific and coherent semantics for our
operations by intuition, allowing our innate sense of geometry to lead.
While it's still not clear *how* actually to implement this library, we have a
much better idea about what it would mean for an implementation to be correct:
it must satisfy every one of our laws stated above!

But before we tackle an implementation, it's worth taking some time to think
about what it means for two tiles to be "equal." The answer can't be
definitional equality, because our equation that `t = cw (cw (cw (cw t)))` is
not equal syntactically. Instead, we decide that two tiles are equal if
and only if they render to equal images --- that is to say, to equal matrices of
pixels. Dealing with on-the-wire byte representations for images is biting off
too much; we deal with *images*, not with GIFs or JPEGs!

Because the semantics of our algebra freely subdivide space, images generated by
it can have arbitrarily precise levels of detail. It would be kind to our users
to expose these features. As such, we should allow users to ask for images of
whatever size they'd like. We can present this functionality through an
*observation*: a function "out" of our algebra.

```haskell
rasterize
    :: Int    -- ^ resulting width
    -> Int    -- ^ resulting height
    -> Tile
    -> [[Color]]  -- ^ pixels in row-major order

data Color
instance Eq Color
```

We say two tiles are equal if and only if they produce the same image under
`rasterize`. More formally:

```{.haskell law="obs eq"}
∀ (t1 :: Tile) (t2 :: Tile).
  (∀ (w :: Int) (h :: Int).
    rasterize w h t1 == rasterize w h t2) => t1 = t2
```

Observations are our users' only means of extracting information out of our
algebra. By saying that equality of tiles is conditional on the equality of
their observations, what we're articulating is that *two things are the same if you
can't tell the difference between them.* This notion of equality is rather
liberating; nobody is saying we need to implement `flipH . flipV` in the same
way as `cw . cw` --- they need only be indistinguishable to our users.

The law `law:obs eq` has a different form than all the others we've seen thus
far. Here there are not one but *two* for-all signs! We can read this as saying
the `t1` and `t2` terms are free to vary like always. But the predicate
must be quantified over the raster's resulting width and height. Two tiles must
be equivalent at *every* size, not just some size. Otherwise, if equality of
only *some* size were necessary, we could choose that size to be zero; two
zero-sized images will always agree because they have nothing to disagree on!
This is a danger of every algebraic model, corresponding to the ever-present
trivial solution to an algebra's system of equations.

Our design isn't complete until we have given laws that constrain the
observation of the algebra. For example, there is absolutely nothing in our
equations that prevent us from mixing up `cw` and `ccw`; every law thus far is
entirely invariant under which is which. Because we will eventually
generate code and tests from our equations here, it's unwise to leave this
ambiguity.

But how can we go about specifying the behavior of observations? In the
same way as we specified our algebra's constructors: through equations. For example, we should
expect the `flipV` operation to move the bottom row of pixels to the top, and
vice versa. More specifically, it should reverse the order of the rows. This is
an easy thing to specify, recalling that our raster is in row-major order:

```{.haskell law="rasterize/flipV"}
∀ (t :: Tile) (w :: Int) (h :: Int).
  rasterize w h (flipV t) = reverse (rasterize w h t)
```

Similarly, `flipH` should flip the pixels within each row:

```{.haskell law="rasterize/flipH"}
∀ (t :: Tile) (w :: Int) (h :: Int).
  rasterize w h (flipH t) =
    fmap reverse (rasterize w h t)
```

To put one tile `above` another, we'd like to split the height in half, and then
concatenate the rows of the top raster with those of the bottom. Special
attention must be paid to the height computations because integers are not
always evenly divisible; here we will make the arbitrary decision that the
bottom raster should soak up the extra tile.

```{.haskell law="rasterize/above"}
∀ (t1 :: Tile) (t2 :: Tile) (w :: Int) (h :: Int).
  rasterize w h (above t1 t2) =
    rasterize w (div h 2) t1 <>
      rasterize w (h - div h 2) t2
```

We can put two tiles `beside` one another in a similar fashion, by gluing
together each row. This is somewhat of a tricky operation, so we will first
convert each raster to column-major, glue the columns together, and then convert
back. The `transpose :: [[a]] -> [[a]]` function can do this major-order
shifting for us. Remember: it doesn't need to be a good implementation, just a
fine specification. Here too, we will decide by fiat that the right-most tile
absorbs the extra pixels if necessary. There is an argument to be made here that
perhaps we should "blend" the middle column, but as we will see in
@sec:tiles-general, this approach doesn't generalize nicely.

```{.haskell law="rasterize/beside"}
∀ (t1 :: Tile) (t2 :: Tile) (w :: Int) (h :: Int).
  rasterize w h (beside t1 t2) =
    transpose $
      transpose (rasterize (div w 2) h t1) <>
        transpose (rasterize (w - div w 2) h t2)
```

The clockwise `cw` operation requires us to rotate our rasterized matrix. I
didn't know of any built-in function to perform this operation, so I
experimented until I found `fmap reverse . transpose` which works, though
admittedly in mysterious ways. Given a matrix:

```{ghci=code/Tiles/Initial.hs}
printSquare [[1,2],[3,4],[5,6]]
```

for whatever reason, we can rotate it via `fmap reverse . transpose`:

```{ghci=code/Tiles/Initial.hs}
printSquare $ fmap reverse $ transpose [[1,2],[3,4],[5,6]]
```

We can thus specify `cw` in this way:

```{.haskell law="rasterize/cw"}
∀ (t :: Tile) (w :: Int) (h :: Int).
  rasterize w h (cw t) =
    fmap reverse (transpose (rasterize h w t))
```

*Notice that the width and height get swapped here.* They refer to the desired
width and height of the *final* raster, and rotating changes which dimension we
call the "width."


Exercise

:  Give a specification for the observation of `ccw`.


Our laws allow us to rewrite `quad` and `swirl` in terms of things we've already
specified, so we will elide them here. All that's left is `color` and `behind`.

The `color` constructor makes a monochromatic image in which every pixel is
identical. This gives us a convenient base case for `rasterize`, which can
simply `replicate :: Int -> a -> [a]` the pixel until the raster is the correct
size:

```{.haskell law="rasterize/color"}
∀ (r :: Double) (g :: Double) (b :: Double) (a :: Double)
      (w :: Int) (h :: Int).
  rasterize w h (color r g b a) =
    replicate h (replicate w (rgba r g b a))
```

where `rgba :: Double -> Double -> Double -> Double -> Color`.

We will hold off on specifying `behind` for now, as it's rather tricky, and as
our immediate analysis will show, turns out to be unnecessary.


### Generalization {#sec:tiles-general}

So, with equality hammered out, are we now ready to implement? Not quite!
Instead, we should ask ourselves if our design is *overfit* --- does it carry
any unnecessary baggage, or ask for unnecessarily strong constraints?

There is a blatant hint that our algebra is overfit --- that the `color` and
`behind` constructors are the only ones in our algebra that require any
notion of colorful pixels.[^why-behind] All of the others, like `cw` and
`beside` are *spatial* operators, useful for moving our pixels around the image
--- but crucially don't depend on any sort of "color" in that capacity. They
could just as effortlessly shape any type of data.

[^why-behind]: What about `behind` requires a notion of color? It's the fact
  that transparent pixels in the second argument are replaced with the colors in
  the first argument!

Taken together, these points suggest that our algebra is attempting to
do two things simultaneously. The first is to act as a sort of "layout manager,"
filling space and organizing things spatially. The second thing our algebra does
is operate on colors, by giving us ways of compositing colors *within that
space.* By teasing apart these related-yet-distinct features, we can generalize
our tile algebra and endow it with significantly more power than its already
appreciable amount.

To separate space from color, we will make `Tile` a functor. Operators
like `flipV` which affect only space can be polymorphic in the type variable on
`Tile`, while `color` and `behind` can be specialized to work only over `Tile
Color`. Under this scheme, every one of our constructors and observations gets a
new type:

```haskell
data Tile a
instance Functor Tile

rasterize :: Int -> Int -> Tile a -> [[a]]

cw     :: Tile a -> Tile a
ccw    :: Tile a -> Tile a
beside :: Tile a -> Tile a -> Tile a
above  :: Tile a -> Tile a -> Tile a
flipH  :: Tile a -> Tile a
flipV  :: Tile a -> Tile a
quad   :: Tile a -> Tile a -> Tile a -> Tile a -> Tile a
swirl  :: Tile a -> Tile a

color  :: Double -> Double -> Double -> Double -> Tile Color
empty  :: Tile Color
behind :: Tile Color -> Tile Color -> Tile Color
```

Functors aren't very interesting on their own; the real value comes from
*applicative functors* and their ability to meaningfully combine a container of
functions with a container of values. Do our tiles admit such a thing? The
behavior for `pure` is analogous to `color` --- to fill the `Tile` with a single
value, everywhere --- but the semantics of the combining operation `(<*>)` are
not immediately evident. When in doubt about semantics, it is often helpful to reason about observations instead.

An interesting perspective to take on observations is that they are
*transformations from an algebra into some other object.* Rather than looking at
`rasterize` with its type `Int -> Int -> Tile a -> [[a]]`, we can do some gentle
fiddling and reorganize its type into this:

```haskell
Tile a -> (Int -> Int -> [[a]])
```

Under this lens, `rasterize` transforms a `Tile a` into a function `Int -> Int
-> [[a]]`. And so, to answer our original question about what semantics the
applicative operation for `Tile` should be, we can instead ask ourselves if
there is some meaningful function `f` with type

```haskell
f
    :: (Int -> Int -> [[a -> b]])
    -> (Int -> Int -> [[a]])
    -> (Int -> Int -> [[b]])
```

which preserves our invariant. Which invariant? That the two integers in each of
these functions correspond respectively to the width and height of the resulting
matrix of values. That is to say, calling `f` should preserve the width and
height of the result.

We are looking for an applicative-like operation that produces a list, and so a
promising place to start looking is at the applicative operation for lists. Such a
thing is already proven to satisfy the applicative laws, and thus is a
particularly desirable candidate. Lists admit two applicative operations: one
which performs the Cartesian product, and one which proceeds element-wise ---
colloquially described as being "substitute-y" and "zippy," respectively.  The
difference between the two is shown below:

```{ghci=code/workflow/initial_algebra.hs}
[(+1), (*8)] <*> [1, 5]
getZipList $ ZipList [(+1), (*8)] <*> ZipList [1, 5]
```

The "substitute-y" operation produces a result of length four when given two
lists of length two --- a disqualifying feature under our invariant of making
the proper-sized result. On the other hand, the "zippy" operation preserves the
size of its inputs. Returning to the land of tiles, this gives us the necessary
insight: our applicative operation should pair two tiles on a per-pixel basis!

In Haskell, any type `f (g a)` can be written as `Compose f g a`, and we can give
a slight variation on `rasterize`:

```haskell
rasterize'
    :: Int -> Int -> Tile a -> Compose ZipList ZipList a
```

which allows us to express the semantics of our applicative operation very
elegantly.

```{.haskell law="rasterize/ap"}
∀ (w :: Int) (h :: Int) (t1 :: Tile (a -> b))
      (t2 :: Tile a).
  rasterize' w h (t1 <*> t2) =
    rasterize' w h t1 <*> rasterize' w h t2
```

The undeniable beauty of `law:rasterize/ap` is our proof that we've
defined `(<*>)` correctly. The technique of "reasoning through the observations"
is one of the **most potent ideas in this book,** and is described more
thoroughly in @elliott_denotational_2009. To paraphrase a slogan from that
paper:

> **The instance's observation follows the observation's instance.**

Equations of this general form like `law:rasterize/ap` are known as
*homomorphisms,* and when specialized over applicatives are called *applicative
morphisms.* Our applicative morphism for `rasterize` shows that it preserves
applicative structure: the applicative operation over tiles is reflected as an
applicative operation in the land of the outputted pixels. This homomorphism
completely specifies that behavior of the applicative instance for `Tile` ---
though it is by no means an implementation in any sense.

To illustrate what this extra generality has bought our algebra, let's introduce
another function `invert :: Color -> Color`, which inverts the individual
channels in a color. We can get a keen sense of this operation from
@fig:fmap_invert_color and @fig:fmap_invert_church, and how we can use our
functor instance to apply it over entire images.

```haskell
invert :: Color -> Color
```

```{#fig:fmap_invert_color design=code/Tiles/Efficient.hs}
fmap invert $ color 1 0 1 1
```

```{#fig:fmap_invert_church design=code/Tiles/Efficient.hs}
fmap invert church
```

Our newfound functor instance allows us to change every pixel in the resulting
image. In contrast, our applicative instance allows us to build a `Tile` whose
"pixels" are functions, and the applicative operation lets us transfer those
functions onto some other image. This behavior can be illustrated as in
@fig:inverted and @fig:applicative.


```{#fig:inverted design=code/Tiles/Efficient.hs}
beside (pure id) (pure invert) <*> church
```

```{#fig:applicative design=code/Tiles/Efficient.hs}
let p1 = pure const; p2 = pure $ flip const; x = quad p1 p2 p2 p1; xx = quad x x x x in behind (beside (color 0 0.8 0.9 1) (color 0.8 0.6 0.2 1)) $ quad xx xx xx xx <*> church <*> flipH church
```

It's interesting to note that `empty` and `behind` form a monoid. In fact, the
monoid that they form is merely lifted from the monoid over color blending.
Rather than hard-coding these operations to act over `Color`, we can instead
generalize them to act over any type that has a `Monoid` instance:

```haskell
empty  :: Monoid a => Tile a
behind :: Monoid a => Tile a -> Tile a -> Tile a
```

and constrain them with the following laws:

```{.haskell law="empty"}
empty = pure mempty
```

```{.haskell law="behind"}
∀ (t1 :: Tile a) (t2 :: Tile a).
  behind t1 t2 = liftA2 (<>) t2 t1
```

