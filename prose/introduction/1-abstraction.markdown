## Overview

### Abstraction

This book is about abstractions --- how to think about them, find good
ones, and wield them effectively. At first blush, this sounds like a
question of style; certainly, abstraction is abstraction is abstraction, right?  No,
not only is this not right; it is not even wrong. When asked what abstraction
*is,* many programmers will give one of the following answers:

1. "It's refactoring: pulling out duplicated code into a reusable function."
2. "It's indirection: providing thin wrappers around calls, and opening the doors
   for future extensibility."
3. "It's parameterization: putting adjustable knobs onto code and solving the
   more general problem."

Each of these is a means to a noble goal, but none of them is
the abstraction tackled here. Instead, we will take a broader, more
encompassing, easier-to-measure definition of what abstraction is. Instead, to
quote @dijkstra_humble_1972:

> The purpose of abstraction is not to be vague, but to create a new semantic
> level in which one can be absolutely precise.

To us, abstraction is only that which creates new, precise semantic domains. But
what does this mean? A helpful abstraction is one which gives us an interpretation
of the world that we accept as being *true.* All ideas are necessarily metaphors ---
reality is just too complicated for human brains. No idea is, therefore, a
ground truth, but an excellent abstraction is one that never reminds you of its
falsehood. A useful abstraction fundamentally changes the way you think and
operate.

Spotting good abstractions is challenging, as they're often mistaken for the way
that the world *really is.* Perfect abstractions are invisible. As an example, the computer hardware world makes excellent abstractions.  Software
doesn't actually proceed one instruction at a time: a modern CPU is decoding
many hundreds of instructions at a time, and invisibly parallelizing them. But
short of extremely-precise out-of-band timing attacks, you can't ever observe
your CPU is doing this. This one-instruction-at-a-time abstraction is so
persuasive that we operate as though it's true, and thus it never violates our implicit assumptions
about how the code works. An excellent abstraction is out of
sight and out of mind.

Likewise, are the fundamental building blocks of your computer really logic
gates? No --- logic gates don't *really* exist; they are an abstraction
describing how transistors behave when placed in specific patterns. But the
concept of logic gates is so persuasive that we collectively forget that are they nothing but
useful figments of our collective imagination.
Treat with suspicion anyone who says abstractions are fundamentally leaky; maybe
these people are just *bad at abstraction.*

Other excellent abstractions are TCP/IP, Boolean algebra, Newtonian physics ---
even mathematics and logic themselves. These are good examples, not because
they're accurate reflections of reality --- but because we *forget that they
aren't.* Contrast this sort of abstraction against what we usually encounter in
software contexts. We're all too familiar with wrappers promising to unify
disparate database interfaces, but which inevitably throw exceptions when one of
the backends doesn't support an operation. Or consider how web-servers encourage
us to think of HTTP headers as key/value pairs, but that a newline character in
either key or value will compromise its entire security model. These are leaky
abstractions, which is to say, worthless ones.

The goal of abstraction is to shield us from the reality beneath. If the real
world somehow manages still to poke through, the abstraction-wielder must be
aware now of both ground truth and the artificial, semantic layer on top. A careful
practitioner now has two sets of invariants he must respect. Simultaneously he
can't be sure of how the abstraction maps to reality or whether the leaks indicate a broken invariant somewhere. In
essence, this wrong abstraction has doubled its practitioner's workload and her
burden of understanding. Take a moment to appreciate just how common this is
when writing software. Any system which gives you a backdoor to escape the
abstraction is necessarily one which admits its incompetence. Computer
systems give us no escape hatch to turn off instruction pipelining, nor can our
programs opt-out of logic gates and instead work directly with transistors. Good abstractions don't require escape hatches.

Our discussion on abstraction is merely foreplay to set the stage for this
book's main contribution: that *code is the wrong abstraction for doing
programming.* There are infinitely many computer programs, the astronomical
majority of which we don't want. There are so many that we *can't want* most of
them. The argument is that unconstrained, implementation-first programming is too expressive as
it's usually done. Code is just too powerful and too
low-level for even the most diligent among us to understand truly any non-trivial
program. Instead, we need better abstractions and tools for the
decomposition, understanding, and solving of problems.

If you take away nothing else from this book, it should be that code is a
uniquely terrible tool for thought. The traditional thought patterns taught in
algorithms class --- and sought out during software interviews --- is
fundamentally detrimental to the problems we're trying to solve. It is a
testament to human heroics and industriousness that we can accomplish so
much in the world of software despite these handicaps. Unfortunately, it's much
more complicated than it needs to be, reflected in part by how
normalized bugs are. Debugging is considered part of the job, and the vast
majority of software has security flaws. The cost
of software also reflects this pain: maintaining software is much more expensive than
writing it in the first place. In most industries, the up-front costs dominate.

Why is this? My answer is that all software tends towards large systems and
that large codebases are impossible for humans to understand in full. Worse,
there are not any widespread tools to aid in that understanding. In an ideal
world, the knowledge from the original design is *reusable,*
and can be reliably shared with others. Imagine a world in which we all
understood a codebase as well as its original author.  Or if we could ask the
compiler to ensure that our invariants always hold. Imagine if the
abstractions never leaked and needing to debug an underlying library were a
thing of the past. Imagine if the code were a byproduct of the understanding,
and wrote itself.

Algebra-Driven Design is a framework for making that future *the future*.

