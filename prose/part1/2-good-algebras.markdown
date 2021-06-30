## What Makes a Good Algebra?

With the tiling algebra successfully tackled, we should take some time to step
back and analyze *why* it worked. What went well, and how can we replicate that
when working in a domain where we have less intuition?

To begin, we had a *scope* for our project in mind. We didn't come into the
project knowing exactly what the finished product would look like, but we knew
we were writing software for generating particular sorts of images. Furthermore,
our goal was usability-based, not implementation-based, which is to say that we
wanted to produce images for humans, not PNGs for computers. Of course, our
design can be used to create PNGs, but that's because PNGs are a particular way
of lossily encoding images[^lossily].

[^lossily]: Why are PNGs lossy? Because they only save the rasterized pixels of
  an image. We can't, for example, take an image, halve its size, save that to a
  PNG, re-open that PNG, and then scale it back to full size. Mathematically such
  an operation should be identity, but we will have lost the
  fine details of any image which undergoes such a procedure. Contrast this
  behavior against vector drawings, which correctly reify transformations like
  these.

Similarly, we didn't come into this design work with an implementation already
thought up. **This is extremely important.** Implementations are tools for
specific jobs. Deciding on an implementation beforehand implicitly
constrains the design, as we must ensure that whatever we design can be
implemented by the uninformed decision. This point is particularly insidious, as
it's the way most programmers tackle most problems, often without realizing it.
Implementation ideas come with baggage: unnecessary constraints, ones which
usually don't compose very well.

With a clear scope in mind, we were in an excellent place to find examples of images
we wanted to be able to build. A diverse set of examples is a powerful thing;
examples help us find missing functionality and allow us to iterate on the
design by coming up with wacky concepts and seeing if they can be built out of
the existing building blocks.

Having sorted out these preliminaries, we were ready to design our algebra in
earnest. We began by listing the "obvious" ways of building an image out of
another, and every time we added a new one, we gave it laws *relating it* to
constructors already established. In doing so, we tried to encode what we
"meant" by each transformation.

The algebra we've built is, first and foremost, *compositional.* Complicated tiles
are built of smaller, simpler ones. Understanding the smaller pieces can be
combined into an understanding of the larger whole, and there is nothing to get in the way of
that understanding. There is no implicit state dictating what a given
operation will do.

The provided constructors all have the property of being *task-relevant:*
they're directly useful to the real humans we expect to be using our software.
That is to say, our constructors all provide high-level functionality, and don't
require "imagination" on the part of the user to build exciting
images. As an extreme contrasting example, consider how the lambda calculus is
*sufficient* to model any computation our users might care about; whether or not
they're ambitious enough to figure out how actually to do so is another question
altogether. By focusing on high-level functionality, we allow our users to build the things they want easily.

But high-level doesn't mean overly-specific! There are not hundreds of
functions, provided "batteries-included" on the off-chance that someone might
want them someday. Instead, our fundamental building blocks are *parsimonious*;
each combinator does one thing and does it well. In the few cases where there
is overlapping functionality (say, `cw . cw = flipV . flipH`), it comes with an
explicit label in the form of an equation, stating that "these two things are
completely indistinguishable." Neither is better than the other, and doesn't
require expert understanding on the user's part to choose one or the other.

Along the same lines, the building blocks of our algebra are *orthogonal* to one
another. There is no concept that is "half-baked" and shared between multiple
combinators. Due to its absence, it's hard to point out, but imagine if
our algebra came with several functions for compositing two images on top of one
another --- one for every possible blending mode. But clearly, the
half-baked idea here is one of how two images should be composited, which we deftly
avoid by providing the infinitely-extensible `(<*>)` operator.

Our algebra is *closed,* meaning that every syntactically valid way of producing
an image is semantically valid. There are no error conditions, and
nothing for our users to be "careful" about. There are no gotchas. Everything
has been carefully chosen so that it works just how you'd expect.

Furthermore, our algebra is *complete,* in the algebraic sense that we have added
as much structure as possible for every reusable concept. When we noticed that
`behind` was associative, we found that it came with a hither-to unseen identity
element `empty`. Where before we had only a binary operation, we now found a
full-blown monoid, providing us with new laws and capabilities that would
otherwise be missing.

Similarly to completeness, our algebra was *generalized.* At first, we noticed that
it worked only on colors, but that this was an unnecessary constraint, and in
fact that most of the combinators we'd discovered were entirely agnostic of
what type was being operated over. Working through the implications of this we
discovered that `Tile` was an applicative functor. Generality allows our
library to be used in situations utterly unlike the examples we had in mind
while building it!

In aggregate, these properties buy us more than we could have imagined. Being
general and complete means the software we've designed is powerful;
orthogonality, parsimony, and interrelatedness show that we've solved the right
problem and task-relevance, closure and compositionality ensure our design is
usable and intended for consumption by human minds. It's a sad commentary on our
world that none of these skills are taught in a traditional computer science
education. Being able to sling code is the bare minimum to do our jobs, but
building powerful, reusable and made-for-humans abstractions is absolutely
critical to do our jobs well.

