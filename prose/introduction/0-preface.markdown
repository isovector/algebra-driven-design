## Preface{-}

For the last seven years, I've been fascinated by what I see to be the central
question in computing --- "why isn't functional programming more popular?" In my
eyes, functional programming (FP) is easier to get right, requires less effort
to produce, and comes with stronger maintainability guarantees than the more
conventional paradigms afford. But if FP is so fantastic, why hasn't it taken
over the world yet?

I can see only three possibilities.

The most obvious one is that functional programming *simply isn't* all that much
better. But this flies directly in the face of my experience and that of my
colleagues in the FP community. There are numerous stories about people coming
from procedural paradigms and falling in love with FP; but very few in which
people go the other direction. Common themes are that functional programming is
more "elegant," and "easier to reason about," and that it "expands our way of
thinking."

Perhaps instead, it's that the market doesn't reward what functional programming
brings to the table. It seems a little far-fetched, but maybe software doesn't
live and die by the speed at which it's written, its correctness, and its
maintainability. By being smaller than its procedural and object-oriented peers,
functional programming languages boast significantly fewer *libraries,* which is
likely part of the issue. It's not that the market doesn't reward speed, merely
that until we achieve library parity, the mainstream inertia will keep its
adherents. There is probably some truth to this. But I don't think this explains
the whole story.

However, the third option is that we FP-people are just not very good at
applying functional principles in large-scale, real-world applications. That is
to say, maybe the problem isn't with functional programming; it's that we
collectively aren't yet good enough with it. It's a common argument that
functional programming works well in the small, but in the large, you actually
need to deal with external systems and real-world complexity. It's in this
interaction with reality that the cracks in FP begin to show.

I think FP's inability to take over the world is this last point. I believe
that, as a community, we're not very good at scaling up functional thinking.
There is no blame here; after all, we all have significantly more experience
engineering procedural systems than we do functional ones. The issue is that it
takes a lot of false starts to move forward. In the mainstream world, these
false starts were already taken by our predecessors. But with functional
programming only now just starting to gain widespread attention, we stand on our
own, with little conventional knowledge to fall back on.

Fortunately, we're not alone in this endeavor. This book presents a
fundamentally different approach for thinking about and writing software, one
which plays to our strengths. It's not a novel idea by any means --- while
researching this book, I found that most of my discoveries were first unearthed
in the mid-70s in the academic community. Academic researchers don't have the
best track record for communicating their research outside of the ivory tower,
and I fear that's what has happened here. An idea is only as good insofar as it
can be acted upon. But it's important to note that the material presented here
is in no way my own research.

To paraphrase Gwern Branwen: if this book has done anything meritorious, it was
perhaps simply putting more work into than someone else would have. My goal has
always been to help communicate better ideas than the ones I come up with.
That's the natural progression of learning, and after a year and two complete
rewrites of this book, boy have I ever learned a lot. I hope you do too.


FlushRight

: Sandy Maguire\
  Victoria, BC, Canada\
  September 2020

