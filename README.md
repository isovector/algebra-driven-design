# Algebra-Driven Design


## Dedication

> Mystery exists in the mind, not in reality. If I am ignorant about a
> phenomenon, that is a fact about my state of mind, not a fact about the
> phenomenon itself. All the more so, if it seems like no possible answer can
> exist.
>
> Eliezer Yudkowsky


## Overview

This repository is all of the original source material for my book
[Algebra-Driven Design](https://leanpub.com/algebra-driven-design/). If you're
curious about what goes into writing a book, it might be a good place to peruse.

Building this thing is particularly hard; I had to write three separate build
tools, and patch a few upstream libraries. You're free to try to figure it out,
but I'd suggest [just buying a copy
instead!](https://leanpub.com/algebra-driven-design/)

Don't make me regret open-sourcing this.


## Commentary

I learned a great number of things when writing [Thinking with
Types](https://leanpub.com/thinking-with-types) --- the first and foremost being
that [liberties constrain](https://www.youtube.com/watch?v=GqmsQeSzMdw). TwT was
written directly in LaTeX, which meant I could layout the page however I wanted,
so long as that page was a standard print size. No dice when it came to making
an epub, only tears.

So instead, Algebra-Driven Design is written entirely in Markdown, and uses
a [custom Pandoc filter](https://github.com/isovector/design-tools) which is
responsible for inlining of code, producing annotations, that funky law style,
and everything else that is typographically intriguing. As a general rule, I've
got NIH syndrome hard, and don't trust software written by other people, so it
was sorta nice to have ULTIMATE CREATIVE CONTROL this time around. At least, I'd
know whom to blame if there was a bug.

All in all, it was a much smoother experience than TwT, and I don't have any
regrets, or things I'd do differently.

People often ask me what the hell this cover is. It's a Lissajous curve, with
the metaphor being "sometimes the right perspective can make complicated things
seem simple." It worked better in my head, but I still like the thing.


## License

This work is licensed under the Creative Commons
Attribution-NonCommercial-NoDerivatives 4.0 International License. To view a
copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or
send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

