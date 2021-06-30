### What is Algebra-Driven Design?

> Programs are meant to be read by humans and only incidentally for computers to
> execute.
>
> --Harold Abelson

Writing software is hard --- likely one of the most difficult challenges that
individual humans have ever undertaken. Our brains aren't wired for it.
We aren't well-adapted for thinking about subtle state interactions that
accumulate over hundreds of thousands of lines of code. Code that is pedantic,
written to describe, in excruciating detail, tasks which are self-evident to
humans. Computers are, by and large, idiots, and the vast majority of a
software engineer's job is understanding problems so well that you can explain
them to these uncomprehending machines.

It is this comprehension that is of paramount importance. As this book argues,
the software engineer's ability to understand problems is her primary
employable skill. The code is merely a byproduct, serving only to explain this
understanding to the computer --- uninteresting in its own right.


While it might sound like a truism to suggest we focus on the understanding of
problems rather than the *programming* of solutions, consider just how difficult
this is with conventional tools and best practices. Our programming languages,
the primary tool for thought for many software engineers, give us no support in
this department. Our only facilities for writing code that is "easily
understood" are to use descriptive variable names, write explanatory comments,
and to optimize our code "to be read" --- whatever that means.

But notice that these are all *code-centric* improvements. At their core, they
still privilege the program as the fundamental unit of study --- the very thing
that is first and foremost an artifact for a computer to execute. The program is
not our understanding of the problem and its solution; it is the end
product of that understanding. If we're lucky, the program comes with comments
that are simultaneously insightful and true, though ensuring this continues to
be the case over time is probably asking too much. For better or worse (but
mostly just for the worse,) documentation is our only tool for preserving
institutional understanding of a codebase. Unfortunately, there is no tool in
existence to ensure our documentation stays in sync with the system it
purports to document. Indeed, tools like `doctest` can help show our examples
produce the output, but there are no tools that check the *prose* of our
comments.

Of course, the program still exists, and in some sense, is the source of truth
of meaning. When documentation goes stale, we are not stuck; we can always reverse-engineer understanding from the program itself. Unfortunately, this state
of affairs is almost synonymous with "programming," at least in most
professional spheres. Over time, software engineers get quite good at this
detective work --- sussing out the "what" from the "how" --- but it is essential
to remember that this is inherently a lossy procedure. Rather than being able to
page in the original author's understanding of the code, we must
reinterpret it, reading between the lines. This is more of an exercise of
psychology than of engineering, as the goal is to recreate another human's state
of mind.

Civilizationally-speaking, we are remarkably successful in this endeavor of
reverse-psychology. It's a testament to all of the heroic maintenance
programmers out there who manage to keep these software systems up and running.
But let's not mince words, this is truly a Herculean undertaking. Programming is a
fundamentally challenging undertaking, yes, but it shouldn't be nearly as hard as it
is. Nor need it be.

The irony here is in our rush to automate away other professions by providing
better tools, by and large, we've forgotten to apply this same mindset to our
own field. Rather than trying to solve underlying problems, which we
are reasonably blind to, we usually find a convenient workaround. For example, why
do we still represent and edit our source code as strings?  Syntactically valid
programs are a vanishingly small subset of all possible strings. The number
of valid edits to a program is minuscule compared to the number of
possible ways we can manipulate a string. Yes, source code does need eventually
to be stored as bytes somewhere on a filesystem. But memory too is just a series
of bytes, and unless you're a low-level C programmer, you almost certainly never
think of memory like that. We don't manipulate instances of objects by
explicitly twiddling their bytes in memory, so why do we still think about the
raw representation of bytes when writing source code? Continually improving text
editors are our convenient workaround here --- but fundamentally, we are still
thinking in terms of bytes, as indicated by our frustration when these tools
"incorrectly" change our formatting.[^lisp]

[^lisp]: Lisp is the notable exception to this point.

My point here is to illustrate that thinking about source code as a string of
bytes is merely working at the wrong level of abstraction. It isn't difficult to
imagine vastly better program editors[^text-editors] than today's best. Tools that edited programs as a tree and which would show only options
that type-checked or were otherwise sane. Tools that would obsolete style arguments by displaying us code in whatever presentation we preferred.
Tools that could automatically test the code we write to ensure it will never
crash (or at least, only crash expectedly) when given garbage inputs.  Imagine
just how spectacular our tooling could be if we stopped insisting on seeing our code as
bytes instead of structured objects that could be manipulated, transformed,
inspected, and generated.

[^text-editors]: It seems more natural to say "vastly better text editors," but
  that would be to miss the point entirely.

A core theme of Algebra-Driven Design is the insistence on working at the proper
level of abstraction and on creating new levels if the available ones aren't
sufficient. This book isn't here to harp on about how source code shouldn't be
represented --- or, for that matter, experienced --- in bytes. No, the argument
presented is that *programs themselves are the wrong level of abstraction,* and
what we can do about that. This book is about learning to focus on the
understanding and offloading most of the coding to our computer tool.

In the same way that having a structured model of source code would enable us to
perform exciting transformations that are infeasible in the land of bytes,
having a *structured model of understanding* affords us a great deal of
flexibility. By reifying our knowledge of what our programs are, we can pass
along *machine-checked* documentation that not only describes our understanding
of what's going on, but is guaranteed to stay relevant (see @sec:quickspec). The
same machinery allows us to *automatically generate* thousands of unit tests ---
not only for properties we understand but also for emergent interactions
between components that are necessary for human understanding of the system at
large (@sec:quickcheck). By having our understanding explicitly modeled, it
becomes an object of study in itself, and we can play around with the
formulation to find better asymptotics or more elegant designs. Perhaps
the most exciting feature of this approach is that it allows us directly to
derive implementations from our understanding, and to discover mind-bending
optimizations that are unlikely to be found by intuition alone.

In short, Algebra-Driven Design is an entirely new way of thinking about
software engineering. To quote @elliott_denotational_2009, whose
thinking on this topic has greatly influenced this book:

> Adopting the discipline illustrated [here] requires additional up-front effort
> in clarity of thinking, just as static typing does. The reward is that the
> resulting designs are simple and general, and sometimes have the feel of
> profound inevitability.

If you are happy toiling in the mire of source code, resigned to semantic games
of "Telephone" between you and all other technicians who have touched a project
over the years, then this is not the book for you. But if the above arguments
have piqued your interest, if you've been dissatisfied with the way things are
done in software, maybe this book can help.

---

This approach has three primary benefits over the usual intuition-driven,
cowboy-coding that often dominates our profession. The first is that it allows
us to focus on the fun part of software design, which is the *design* and
understanding of the problem. It spares us most of the work of dealing with the
incidental complexity, interfacing with clunky libraries, figuring out how
exactly to decompose our dependency injection, and writing unit tests. These
things are necessary for a working, shippable program, but they aren't
software engineers' comparative advantage: understanding and dealing with
complex systems.

As a byproduct of reifying our thinking, we find the second benefit: that we can
offload a great deal of our work to the computer tool. By having
machine-checkable artifacts, the computer can tell us when our laws combine in
nonsensical ways, when our reference implementation doesn't do what the laws
say, or when our real implementation doesn't line up with the reference. By
doing this thinking outside of our heads, we can ask the computer to help suggest laws we might
have missed, and generate thousands of unit tests following our specification
--- testing for edge cases that no human could ever consider
"manually."

Having saved the best for last, the final benefit of ADD is that it allows us to
derive implementations, if not "for free," then at least "greatly discounted."
The equality laws guiding us through this entire process are
excellent at finding the best "carve" of implementation through the design. The
resulting programs are often beautiful, and more often than not, feel
*discovered* rather than *engineered.* Through this approach, programs are
elegant in their simplicity and generality, and playful manipulation of the
laws is eerily good at finding asymptotic improvements. In essence, this
means that the approach is a reliable generator of insights --- both in
design work and in coming up with intelligent optimizations.

In a genuine sense, Algebra-Driven Design is about designing
programs *for humans.* The unreasonable effectiveness of mathematics seems
related to the fact that it models things that humans can understand formally.
Algebra is not a study of the world; it is a study of psychology. It is the
study of those systems we humans can think clearly about.

With all of these significant advantages of Algebra-Driven Design, it's necessary to
realize what it is *not.* ADD is incredibly helpful in finding reusable
abstractions, which is to say, it's good when designing libraries. Applications
are particular instantiations of reusable library components, and if you insist
on thinking of an application as completely non-reusable code, ADD cannot help
you. Instead, it encourages separating the core logic --- the bits that make
your program *your* program --- from the chaff of programming tasks
necessary to connect your program to the outside world. Strong adherence to
ADD pushes library code to the forefront and resigns applications to thin
wrappers around library functionality.

If you were never particularly good at math in school, take heart! Despite
having the word "algebra" in its title, Algebra-Driven Design is not about the
sort of algebra that causes nightmares. The algebra here has nothing to do with
numbers, nor with arcane rules handed down by fiat from on high. The word
"algebra" specifies that we are working algebraically, which is to say,
thinking about what equations should hold, and manipulating those rules to find
answers. Those answers? The implementations of the programs we want to write.

Furthermore, Algebra-Driven Design has nothing at all to do with visual
aesthetics. It is not about "designing a good user experience" or with
"designing a pleasing webpage." ADD is about designing excellent *software* --- which
is to say, software that is easy to understand, guaranteed to do what it says on
the tin, and flexible enough to be used in more ways than its original authors
ever could have intended.

Algebra-Driven Design isn't particularly prescriptive. It's an interactive
process, a discussion between your tools and
your intuition, helping refine ideas until they're beautiful.

Finally, Algebra-Driven Design need not be a solo endeavor. The entire point of
giving laws and models is to share your understanding with others. Doing a
formal review with your team of the design is a great way to find missed
opportunities for additional structure or more elegant decompositions.
Furthermore, the design acts as a living document, as machine-verified
documentation, which gives future maintenance programmers (perhaps including
yourself) a direct-link into your mind at the time you created the system, as
well as to any improvements which have happened over the years.

