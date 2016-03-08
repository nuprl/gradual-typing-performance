#lang scribble/base

@; TODO more story, more wandering in the midst of the lattice, more anti-paths

@require["common.rkt" "typed-racket.rkt"]
@(require racket/file benchmark-util/data-lattice)

@title[#:tag "sec:story"]{Gradual Typing in Typed Racket}

Our research is motivated by practical experience with Typed Racket@~cite[TypedRacket],
 the gradually-typed sister language of Racket@~cite[plt-tr1].
Typed Racket's users have built diverse applications including plotting libraries,
 web servers, probabilistic programming languages@~cite[tmv-esop-2014],
 and music studios.@note{Links and references to users' code are available in
   the online supplement to this paper.}
Some of these programs migrated to Typed Racket from untyped Racket and others
 were typed projects from the start.
At any rate, gradual typing has become a fundamental part of the Racket ecosystem.
Many popular libraries are typed, thus their untyped clients are implicitly
 gradually typed programs.

The freedom to mix typed and untyped code has proven useful for finding
 bugs and strengthening APIs, but Typed Racket programmers frequently
 encounter surprising performance costs.
Recent complaints include a 50% slowdown from mixing typed and untyped code
 in a commercial web server,
 a 10x slowdown after fully typing a program in the hope of improving performance,
 and a performance degredation from 1 millisecond to 12 seconds when
 calling a typed data structures library from untyped Racket.
Typed Racket's math library even includes a disclaimer about potential
 25x-50x slowdowns in untyped programs.

@; Source of costs
Boundaries between typed and untyped code are the source of the performance costs.
As a concrete example, consider a function for multiplying two complex numbers
 represented in polar form:

@(begin
#reader scribble/comment-reader
(racketblock
  (define-type C (Pairof Nonnegative-Real Real))
  ;; C = (Distance from origin, Radians)

  (: complex-* (C C -> C))
  (define (complex-* c1 c2)
    (cons (* (car c1) (car c2))
          (+ (cdr c1) (cdr c2))))
))

When this typed function is imported by an untyped module, we say it crosses a
 @emph{type boundary}.
During the import, the function's type signature is compiled into a
 higher-order contract@~cite[ff-icfp-2002] to dynamically enforce correct usage.
Specifically, the contract asserts that each call to @racket[complex-*]
 in untyped code is made with two arguments matching the type specification
 for complex numbers.
Incorrect calls like @racket[(complex-* '(1 . 1) "NaN")]
 are detected immediately, before @racket[complex-*] is invoked, and
 the untyped code is blamed@~cite[ff-icfp-2002] for making a type error.@note{
   Correct blame is essential to Typed Racket's soundness guarantee.
   Typed code will never cause or be blamed for run-time type errors.}
Conversely, typed modules may import untyped code by assigning types to untyped
 identifiers.
Assigned types are compiled to contracts and dynamically ensure that the
 untyped code meets its type specification.


Note that pairs are structured data, so
 validating a single argument to @racket[complex-*] requires three assertions.
These assertions are relatively fast on their own, but dynamically checking
 whether a large list contains only complex numbers or whether an untyped
 function always returns complex numbers can become arbitrarily expensive
 as the list grows or the function is called repeatedly.

@; TODO example here?
@; The program in @Figure-ref{TODO} underscores this point.
@; It is a script for (... tree / list ... complex ...)
@; When the script is typed, it runs in XXXms. Untyped, the same code takes
@;  YYYms to run.
@; TODO connect example to soundness?

Despite the potential cost, it is essential that each call to @racket[complex-*]
 is guarded against type errors.
Although certain inputs will be caught by low-level operations---for example,
 @racket[car] and @racket[+] dynamically check their arguments---ill-typed calls
 like:
    @racketblock[(complex-* '(-1 . -1) '(-3 . 0))]
 fail @emph{silently}.
If we are lucky, the silent failure will trigger an error later in the program,
 but since the polar product of @racket['(-1 . -1)] and @racket[(-3 . 0)] is
 the well-typed complex number @racket[(3 . -1)] it is more likely that the
 program will compute an incorrect result and leave the programmer no clue as to
 where the error occurred.
Such are the dangers of committing moral turpitude@~cite[r-ip-1983].


@; -----------------------------------------------------------------------------
@section[#:tag "sec:fsm"]{Understanding Software Complexity}

Knowing that type boundaries introduce run-time checks explains
 why gradually typed programs are often slower than fully-typed or fully-untyped
 programs.
This information is useful, but from a programmer's perspective the real question
 is why a given program is slow and what can be done to improve performance.

In practice, a gradually typed program consists of @math{N} modules under
 the programmer's direct control.
The program could be anything from a one-module script to a large software
 system with hundreds of intertwined modules.
Occasionally the program is independent, using only core Racket or Typed Racket
 libraries.
More often, the program depends on other libraries in the Racket ecosystem.
These dependencies often introduce type boundaries, either through direct
 interaction with the program or through their own dependencies.
@todo{examples}
The point is that from the beginning, the boundary structure of a program
 may be large and complex.


@; Awkward, just trying to justify why the set of typed programs is random
A subset of the @math{N} modules in the program will be typed.
This subset is essentially chosen at random.
Having given programmers the freedom to add types where convenient, we find that
 Typed Racket users have diverse motivations.
Here are some of the most common use-cases:
@itemlist[
  @item{ @bold{Assurance:}
    The typed modules implement core functionality that the programmer
     wanted to guard against simple bugs; for example, forgetting to check
     for end-of-file symbols when reading from a port.
  }
  @item{ @bold{Documentation:}
    The typed modules are stable, whose interfaces are unlikely to
     change.
    Type signatures serve as machine-checked and enforced APIs.
  }
  @item{ @bold{Experimental:}
    The typed modules were the least effort to type.
    These could be the smallest modules or the ones with the fewest dependencies.
  }
  @item{ @bold{Necessity:}
    The typed modules are tightly-coupled to other typed modules or to typed
     library code.
    That is, the programmer has identified an expensive type boundary and
     removed the performance cost by typing additional modules.
  }
]
A particular module may be typed for one, many, or none of the above reasons.

We might hope that all gradually typed programs are in a temporary state
 and will soon be fully typed, but this hope is often in vain.@note{As a
     technical point, certain untyped features cannot be used in Typed Racket,
     but this set of features is a small and shrinking.}
Programmers' main task is to deliver a working and reasonably correct software
 product.
Adding type annotations to untyped code is @emph{orthogonal} to this goal,
 especially when the untyped code is well-tested and appears to work correctly
 in practice.
Types may catch small bugs or ambiguities in an API, but their net benefit is
 frequently not worth the software engineering cost of recovering and writing
 out the type annotations.
In a codebase with hundreds of modules, it is unrealistic to expect that programmers
 will devote time to typing each line of code---indeed, the crucial feature of
 gradual typing is that developers need not type everything before receiving
 the benefits of static types.

On the other hand, if an untyped module exhibits a bug, the first step in
 fixing the incorrect behavior is understanding implicit type signatures in the module.
These signatures may be on function domains or characterize values flowing
 along program paths.
Either way, the programmer debugging untyped code must recover this type information
 in order to identify and prevent future bad values from triggering a bug.
A gradually typed language encourages programmers to record these types, thereby
 protecting against future issues; however, adding types to a single module
 introduces new type boundaries.
As language designers, the lesson here is that gradual typing is self-propogating:
 programmers are encouraged to add types exactly where useful and never across the
 entire program.

Finally, when programmers ask for advice about performance issues they
 have a target performance in mind.
The target is usually specified in terms of how a fully-untyped version of
 the program runs.@note{Although adding types is difficult, ignoring all types
 in a Racket program is a 1-line change.}
When the program is under heavy development or in an intermediate state,
 order-of-magnitude slowdowns may be acceptable.
That is, a programmer may be willing to tolerate a 10x slowdown as long as
 the unit tests will run.
Performance requirements for the final version of a program are typically much
 stricter.
Even 2x overhead relative to the untyped program may be unacceptable.
Useful feedback for these programmers is a list of untyped modules that, if
 typed, would mitigate the performance overhead.

@; -----------------------------------------------------------------------------

To make the above points concrete, we examine a 6-module Racket program,
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
 generated all 64 possible ways of gradually typing the program.
To each of these 64 configurations we assign a bitstring corresponding
 to @bm{suffixtree}'s module names in alphabetical order.
Using this notation, the fully-untyped configuration is @bits{000000}
 and the configuration with only @tt{data} and @tt{node} typed is @bits{100010}.

After generating these configurations, we ran each for 30 iterations and
 recorded the average running time.@note{Our experiment protocol is detailed in
  @Secref{sec:protocol}, where we present similar results for @id{(- NUM-BENCHMARKS 1)}
  other programs.}
These results are organized in an annotated @emph{performance lattice} in
 @Figure-ref{fig:suffixtree-lattice-6.2}.
The bottom level of the lattice has the untyped configuration; the first
 level has all configurations with exactly one typed module, and so on to the top.
Every node in the lattice is marked with a sequence of colored shapes representing
 a configuration's bitstring.
A black shape represents a typed module and a white shape is an untyped one;
 in other words, the shape at index @math{3} from the left is colored
 iff the bit at index @math{3} from the left is 1,
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
     avoids worst-case overheads within 20x.
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

In terms of our earlier discussion, a programmer who converts a random subset
 of @bm{suffixtree} to Typed Racket is very likely to arrive at a configuration
 with high performance overhead.
Moreover, there seems to be little hope that typing one or two additional
 modules can possibly recover performance.


