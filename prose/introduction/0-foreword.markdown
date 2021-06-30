## Foreword{-}

### The Restoring of Broken Parts{.unlisted .unnumbered}

Algebra. We all studied it at school. We learned "algebraic laws" such as
commutativity

$$x + y = y + x$$

and associativity

$$(x + y) + z = x + (y + z)$$

so well that applying them became second nature, and we could use them in long
reasoning about equalities, without any need for more complicated proof
techniques such as proof-by-cases, proof-by-induction, or
proof-by-contradiction.

We may think of algebra in connection with proofs, but it is not only useful for
reasoning. It also lets us *abstract away* from unimportant details. Every time
we write $a + b + c + d$ without worrying where the brackets should go, we are
taking advantage of the associative law---the second equation above---and
thinking at a higher level of abstraction. This matters to mathematicians, and
it also matters to programmers. Every time you sum an array in a loop,

```c
int sum(int a[], int n)
{ int sum=0;
  for(int i=0;i<n;i++) sum = sum + a[i];
  return sum;
}
```

you are relying on associativity, to guarantee that it doesn't matter which
*order* you combine the array elements in. When laws break down, there are
problems. For example, *the code above is actually incorrect* for summing an
array of floats! Floating point addition is *not* associative, and if you add up
a million floats using a loop like this one, *you will get the wrong answer!*
Instead you can use a clever algorithm called 'Kahan summation' to get a much
better one (look it up; next time you have a million floats to add up, you'll
thank me). But the lesson is this: when algebraic laws fail, the ground wobbles
under our feet.

Computer scientists have been interested in the "*algebra of programs*" for more
than half a century. In his classic 1966 paper 'The next 700 programming
languages', Peter Landin wrote:

> For most programming languages there are certain statements of the kind,
> 'There is a systematic equivalence between pieces of program like this, and
> pieces like that,' that nearly hold but not quite. ...  At first sight it
> might appear pedantic to quibble about such untidiness---'What's the point of
> having two different ways of doing the same thing anyway? Isn't it better to
> have two facilities than just one?' The author believes that *expressive power
> should be by design rather than accident*, and that there is great point in
> equivalences that hold without exception.

The desire for "equivalences that hold without exception" is one of
the strong motivations for functional programming; that $x - x = 0$,
but

```c
getchar() - getchar() == 0
```

will usually be false in C, is a great impediment to algebraic reasoning.

However, the algebra of programs extends far beyond the usual algebra
of numbers. In his 1978 Turing Award lecture, John Backus argued that
the very *constructions of a programming language*, which he
called "functional forms", should be chosen to support algebraic
reasoning:

> "One chooses only those functional forms that not only provide powerful
> programming constructs, but that also have attractive algebraic properties:
> one chooses them to maximize the strength and utility of the algebraic laws
> that relate them to other functional forms of the system."

Backus advocated high-level combining forms such as `map` and `reduce` (fold),
rather than low-level constructions such as sequencing or iterating statements,
which is echoed in the "point-free" programming style popular in Haskell today.

Backus favoured algebra, over other kinds of reasoning, because it is
*easy* and *practical*:

> The algebra of the programs described below is the work of an amateur in
> algebra, and I want to show that it is a game amateurs can profitably play and
> enjoy, a game that does not require a deep understanding of logic and
> mathematics. In spite of its simplicity, it can help one to understand and
> prove things about programs in a systematic, rather mechanical way.

Indeed, we can reason *algebraically* about programs, that one program is the
same as another, without *any* external notations or tools. All we need is some
middle school mathematics.

We can go on to apply algebra not only to numeric expressions and to programming
language constructions, but to *all* our APIs! In their classic 1978 paper 'The
Algebraic Specification of Abstract Data Types', Guttag and Horning argue that
algebraic laws are the right way to *specify* the behaviour of an API:


> The set of axioms defines the meaning of the operations by stating
> their relationships to one another.
>
> They are easy to read and comprehend, thus facilitating informal verification
> of the fact that they do indeed conform to the intent of their creator.


When an API satisfies a rich set of laws, then the *algebra gives us freedom*:

* We're free to think at a higher level of abstraction, without worrying about
  trivial differences that the algebra assures us don't matter.
* We're free to optimize one piece of code to another with better performance,
  without risking introducing a bug, because the algebra assures us the two are
  equivalent.

Algebraic laws are a powerful approach to optimization, a key part of Burstall
and Darlington's program transformation method, in their classic 1977 paper 'A
Transformation System for Developing Recursive Programs'. Today you can even
give performance-enhancing algebraic laws about an API to the Glasgow Haskell
compiler, to be applied automatically by the optimizer to speed up client
programs whenever opportunity arises.

If algebraic laws are so useful, then clearly it is highly *desirable* that an
API should satisfy many of them. That means that algebra can serve as a
*touchstone* for good design. In 1982 Peter Henderson invented "functional
geometry", a simple API for describing complex pictures, with a beautifully
simple algebra. Henderson later wrote: "It seems there is a positive correlation
between the simplicity of the rules and the quality of the algebra as a
description tool." You will learn all about Henderson's algebra later in this
book; remember his advice---when choosing between design alternatives, choose
the one with the better algebra!  If your algebra is strong enough, you may even
be able to *calculate the implementation* using it.

I experienced all this myself in the early 90s, when working on a library for
writing pretty-printers in Haskell. Almost every data-structure needs a
pretty-printer, and I was fed up writing the same kind of code over and over
again, and making the same kinds of mistakes over and over again---getting the
layout of pretty-printed output right is surprisingly tricky. So I designed an
API for pretty-printers, but it wasn't crystal clear what each operator should
do, with the result that my pretty-printers were *still* buggy in weird cases!
But now that I had an API, I could look for algebraic laws---and tweak the
meanings of the operators to satisfy them. That clarified the design
wonderfully, and eliminated my bugs. I was even able to use my algebra to
*calculate* a highly-efficient implementation, resulting in code that I could
never have written by hand---with confidence that the algebra would ensure that
all the weird corner cases were correctly handled. The paper I wrote as a
result, 'The Design of a Pretty-printing Library' (1995) has spawned an
entire mini-field of algebraically-based pretty-printing.

Of course, when you try to apply algebra to your own code in practice,
you will immediately ask yourself questions like

* How do I know my code really satisfies this law?
* How can I figure out *which laws* my code satisfies?

Fortunately, today there are tools for Haskell that can answer these questions
for you---QuickCheck to test that a particular law is satisfied, and QuickSpec
to *find* a set of laws in the first place. So we are in a much better position,
today, to apply algebraic methods to our code in practice, than the pioneers I
quoted above.

But to apply these methods yourself, there is much to learn---and this is what
this lovely book will teach you. What algebra is, how to apply it, what to look
for, how to let it guide your designs, how to calculate your code, how to use
the tools that are now available to support the approach. It's all illustrated
with a new take on Peter Henderson's functional geometry, and a much
larger-scale and more realistic game application, with a down-to-earth approach
that you can put straight into practice. I am so glad that Sandy has written
it---it provides an excellent introduction to so many ideas that I love.

Algebra enabled me to turn my pretty-printing code from a useful-but-buggy mess
into a thing of beauty.  The word 'algebra' itself comes from the work of
Persian mathematician Muhammad ibn Musa al-Khwarizmi in the ninth century, and
means "the restoring of broken parts". I think it's quite appropriate.

May this book teach you how to restore broken parts.


FlushRight

: John Hughes\
  Gothenburg, Sweden, September 2020.

