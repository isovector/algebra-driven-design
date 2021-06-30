### A Note on the Companion Library

All of the algebras designed in this book are available online in the companion
Haskell library, available at
[https://hackage.haskell.org/package/algebra-driven-design](https://hackage.haskell.org/package/algebra-driven-design).
It exists as a bit of a compromise, and if at all possible, I would encourage
you to *not use it.*

Many of the early beta readers of this book expressed difficulty in holding the
entirety of the large algebras in their heads; many of them wanted a tangible
example they could play around with to build intuition. This is an
extremely reasonable request, and one I can sympathize with --- it's a fantastic
way of developing an understanding of a new system!

Unfortunately, out there in the real world when we're building these
systems, there isn't anything to play around with, because we haven't
made it yet! This problem isn't unique to algebra design, but the usual
solution to build small pieces of it at a time and play around with those *is*
antithetical. Due to some weird quirk of the human mind, code that exists is
unlikely to be scrapped, even if it's not the code you'd have written after
thinking about the problem for an hour or two.

Your primary goal as you work through these pages should be to train yourself to
internalize the semantics by running the designs on your brain hardware, rather
than requiring it to be done on metal. I will readily agree that this technique
is not suitable for the vast majority of software you have encountered
throughout your career, but the systems we will build in this book are
different. You'll see why: we altogether eschew implementations, and there
is never any non-local state to keep track of.

Instead, one of these algebras' main selling points is that they have been
consciously designed to be digestible by human minds. This comes from one
particular feature of the design process: that small pieces' semantics are
composable into larger pieces. The macroscopic features are built out of
well-understood smaller pieces and are guaranteed never to have any "gotchas"
in how they behave at scale. By internalizing the handful of tiny building
blocks, we can accurately determine any composite program's behavior. It's not
magic; it's just that we carefully designed these systems. This doesn't mean
the implementation can't be complicated --- only that the interface
necessarily be simple. Indeed, as we will see, it's impossible for any
implementation details to "leak out" and get in the way of our understanding.

So, consider the companion library to be a crutch, one that is available, but
should be used only in dire circumstances. If you still have no idea what a function is supposed
to do after a few concentrated readings of the relevant paragraphs, then reluctantly fire up the library and play around with it. But first,
predict what it might do and *write it down.* Then work
to confirm or deny that hypothesis. As soon as you understand
this one piece's semantics, I implore you, put the library away, and continue through
the prose. Modeling *how software should work* mentally is one of the
most crucial skills in this book. I promise not to betray your trust that the
code presented here behaves precisely like your mental model says it should, and
in return, I pray you'll put in the work. If you come away from this book and it
hasn't changed how you approach writing software, then I've failed, and
you've wasted your money. Let's not let that happen.

