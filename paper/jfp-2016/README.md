jfp
===

 Typed Racket to the object-oriented aspects of Racket, in which we
 - use a lattice-style approach for a preliminary performance evaluation
 - conducted in tandem with the design and implementation@~cite[tfdffthf-ecoop-2015].
 -By inspecting all possible ways of typing two small
 - game systems, we identified and then eliminated a major
 - performance bottleneck from the implementation.
 -In contrast, we conduct our current evaluation completely @emph{independently} of
 - Typed Racket's implementation efforts.@note{In terminology
 -  borrowed from the
 -  education community@~cite[scriven-chapter-1967], we conducted a @italic{formative
 -  evaluation} while this paper conducts a @italic{summative
 -  evaluation} to assess the post-intervention state of the system.}
 -Thus our framework is directly applicable to other sound gradual type systems.

@; -----------------------------------------------------------------------------

@; Different inputs
As a final note, our experiment runs the benchmarks using fixed inputs,
 but the results should be the same on different inputs.
We have in fact experimented with inputs of various size and content
 for select benchmarks but found
 the relative overheads due to type boundaries remained the same.
For the purpose of the experiment, the final input size we used was a compromise
 between having an untyped runtime long enough to be stable against
 operating system effects but short enough that the slowest
 configurations finished reasonably quickly.

@; performance guidelines
To make the above points on performance concrete, we examine a 6-module Racket program,
 @bm{suffixtree}, that implements a longest-common-substring function
 using Ukkonen's suffix tree algorithm@~cite[u-algorithmica-1995].
The program consists of six modules:
@itemlist[
 @item{@tt{data} defines label and tree nodes,}
 @item{@tt{label} defines functions on suffixtree node labels,}
 @item{@tt{lcs} computes longest common substrings,}
 @item{@tt{main} provides input to @tt{lcs},}
 @item{@tt{node} creates and traverses suffix tree nodes,}
 @item{and @tt{ukkonen} builds suffix trees.}
]
Originally, @bm{suffixtree} was an untyped program.
We manually added type annotations to the entire program by reading the
 developer's comments and reasoning about known inputs to the program.
From the fully-untyped and fully-typed versions of @bm{suffixtree} we then
 generated all gradually typed configurations.
@; TODO what is a configuration?
To each of these 64 configurations we assign a bitstring corresponding
 to @bm{suffixtree}'s module names in alphabetical order.
Using this notation, the fully-untyped configuration is @bits{000000}
 and the configuration with only @tt{data} and @tt{node} typed is @bits{100010}.

After generating these configurations, we ran each for 30 iterations and
 recorded the average running time.@note{Our experiment protocol is detailed in
  @Secref{sec:protocol}, where we present similar results for @id[(- NUM-BENCHMARKS 1)]
  other programs.}
These results are organized in an annotated @emph{performance lattice} in
 @Figure-ref{fig:suffixtree-lattice-6.2}.
The bottom level of the lattice has the untyped configuration; the first
 level has all configurations with exactly one typed module, and so on to the top.
Every node in the lattice is marked with a sequence of colored shapes representing
 a configuration's bitstring.
A black shape represents a typed module and a white shape is an untyped one;
 in other words, the shape at index @math{3} from the left is colored
 if and only if the bit at index @math{3} from the left is 1,
 meaning the @tt{main} module is typed.
Nodes are labeled with the configuration's overhead---computed
 as the configuration's mean runtime divided by the fully-untyped
 configuration's mean runtime---and
 the standard error of our timings.

@figure*["fig:suffixtree-lattice-6.2"
  @list{Annotated performance lattice for @bm{suffixtree} (Racket v6.2)}
  @todo{(data-lattice 'suffixtree "6.2")}
]

Examining the lattice raises a number of interesting points.
@todo{BEGIN check numbers}
@itemlist[
  @item{
   The fully typed configuration runs faster than the fully untyped
    configuration by approximately 30%.
   This improvement is due to specialization of arithmetic operations and
    field accesses by Typed Racket's optimizer @~cite[thscff-pldi-2011].
  }
  @item{
   Almost all gradually typed configurations exhibit large slowdowns,
    up to 105x.
  }
  @item{
   Adding types to any of @tt{data}, @tt{label},
    or @tt{node} while leaving all other modules untyped causes slowdown of
    at least 35x.
   Not surprisingly, these modules are tightly coupled in the program.
  }
  @item{
    Not a single path from untyped to typed converting one module at a time
     avoids worst-case overheads below 20x.
  }
  @item{
   The five slowest configurations are @todo{list}.
   These all have a type boundary between the first two modules: @tt{data} and
    @tt{label}.
   Configurations where @tt{data} and @tt{label} have the same type
    have an average overhead of @todo{X}.
  }
]

The performance lattice for @tt{suffixtree} is bad news for gradual typing in
 Typed Racket.
It exhibits many performance ``valleys'' in which a group of similar configurations
 all suffer large performance overhead.
Consider starting with the untyped program and choosing
 to add types to @tt{label}.
The program slows down by a factor of 88x.
Without any guidance, a developer may then choose to add types to @tt{node};
 now the program slows to 104x.
After that, typing @tt{main} (104x), @tt{ukkonen} (99x), and @tt{lcs} (103x)
 do little to improve performance.
It is only when all the modules are typed that performance becomes acceptable
 again (0.7x), at which point it is probably too late to convince the programmer
 that gradual typing is useful.
@; Useful for more than the static check of making sure intermediate programs type-check

@todo{END check numbers}

In terms of our earlier discussion, a programmer who converts a subset
 of @bm{suffixtree} to Typed Racket is very likely to arrive at a configuration
 with high performance overhead.
Moreover, there seems to be little hope that typing one or two additional
 modules can possibly recover performance.


@; -----------------------------------------------------------------------------

Finally, when programmers ask for advice about performance issues, they
 have a target performance in mind.
The target is usually specified in terms of how a fully-untyped version of
 the program runs.
When the program is under heavy development or in an intermediate state,
 order-of-magnitude slowdowns may be acceptable.
That is, a programmer may be willing to tolerate a 10x slowdown as long as
 the unit tests will run.
Performance requirements for the final version of a program are typically much
 stricter.
Even 2x overhead relative to the untyped program may be unacceptable.
Useful feedback for these programmers is a list of untyped modules that, if
 typed, would mitigate the performance overhead.

Knowing that type boundaries introduce run-time checks explains
 why gradually typed programs are often slower than fully-typed
 or fully-untyped programs.
This information is useful, but from a programmer's perspective the real question
 is why a given program is slow and what can be done to improve performance.
The purpose of this section is to explain what we can assume about the
 so-called @emph{given program} (spoiler: we cannot assume much).
@; TODO bad sentence, need to replace with something BUT WHAT

Typed Racket's users have built diverse applications including
 plotting libraries,
 probabilistic programming languages@~cite[tmv-esop-2014],
 music studios,
 and
 web servers.@note{Links and references to users' code are available in the online supplement to this paper.}

---

- GCTIME
  - Emery : https://people.cs.umass.edu/~emery/pubs/gcvsmalloc.pdf
    - 19% with 3x heap
    - 70% with smaller heap
  - ???   : https://people.cs.umass.edu/~immerman/pub/hertz.pdf
    - 20% "acceptable for personal computers"
  - Ellis, Appel, Li : http://www.hpl.hp.com/techreports/Compaq-DEC/SRC-RR-25.pdf
    - 
  - ???   : /Users/ben/Downloads/mmtk-sigmetrics-2004.pdf 
    - 14% is high
    - 
  - ???   : http://www.cs.utexas.edu/users/mckinley/papers/mmtk-icse-2004.pdf
  - Ungar https://people.cs.umass.edu/~emery/classes/cmpsci691s-fall2004/papers/p157-ungar.pdf

- use confidence intervals when reporting measn
  https://www.cs.kent.ac.uk/pubs/2012/3233/
  (don't be a statistic in future statistics papers)
- better example in intro
- fix acquire, put tree boundary back
- lattice clearly leaves questions unanswered
  (which modules to type next?)
  (cost/boundary)

---


quarantine the refs. to "typed racket"
run-time vs runtime vs run time
typecheck vs type-check vs type check
vary inputs, confirm the same lattice
benchmark vs program
no rhetorical questions


--- 

It's not about paths. It's a random walk.
You can't do a path in i.e. dr racket, with 20,000 (?) modules
- "no paths if we do not tolerate overhead along the path"
- "never start at bottom & go to top, only make a few steps, different scenario"
- just academic question to try & find a best path

"Not all paths are equally likely"
WE DONT KNOW. We really don't know! Really!



Building
---
- Change `scribble/jfp` to look for `jfp.cls` instead of `jfp1.cls`
- Download `ilfonts`, then `\usepackage{ilfonts}`

---

 @;dynamic features@~cite[tsth-esop-2013 vksb-dls-2014 sfrbcsb-popl-2014] in gradual type systems,
 @;continuation-passing@~cite[tsth-esop-2013],
 @;object identity checks@~cite[vksb-dls-2014], and
 @;run-time evaluation@~cite[sfrbcsb-popl-2014],
