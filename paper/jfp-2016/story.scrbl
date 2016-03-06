#lang scribble/base

@; TODO more story, more wandering in the midst of the lattice, more anti-paths

@require["common.rkt"]
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
  ;;; C = (Distance from origin, Radians)

  (: complex-* (C C -> C))
  (define (complex-* c1 c2)
    (cons (* (car c1) (car c2))
          (+ (cdr c1) (cdr c2))))
))

When this typed function is used in untyped code, its type signature is
 interpreted as a higher-order contract@~cite[ff-icfp-2002].
The contract asserts that each call to @racket[complex-*] in untyped code
 is made with two arguments matching the type specification for complex numbers.
Incorrect calls like @racket[(complex-* '(1 . 2) "NaN")]
 are detected immediately, before @racket[complex-*] is invoked, and
 the untyped code is blamed@~cite[ff-icfp-2002] for making a type error.@note{
   Correct blame is essential to Typed Racket's soundness guarantee.
   Typed code will never cause or be blamed for run-time type errors.}

Note that pairs are structured data, so
 validating a single argument to @racket[complex-*] requires three assertions.
These assertions are relatively fast on their own, but dynamically checking
 whether a large list contains only complex numbers or whether an untyped
 function always returns complex numbers can become arbitrarily expensive
 as the list grows or the function is called repeatedly.

Despite the potential cost, it is essential that each call to @racket[complex-*]
 is guarded against type errors.
Although certain type errors will be caught by low-level operations---for example,
 @racket[car] and @racket[+] dynamically check their arguments---ill-typed calls
 like @racket[(complex-* '(-1 . -1) '(-3 . 0))] fail @emph{silently}.
If we are lucky, the silent failure will trigger an error later in the program,
 but since the polar product of @racket['(-1 . -1)] and @racket[(-3 . 0)] is
 the well-typed complex number @racket[(3 . -1)] it is more likely that the
 program will compute the wrong result and leave the programmer no clue as to
 where the error occurred.
Such are the dangers of committing moral turpitude@~cite[r-ip-1983].


@; -----------------------------------------------------------------------------
@section[#:tag "sec:fsm"]{A Larger Example}

Even programmers who know the pitfalls of 


Having established that Typed Racket has known performance issues
 and given our opinion that we must address rather than avoid these issues,
 we now examine the boundaries occurring in a small program.
Our running example for this section is a small program, @tt{fsm},
 that simulates a population of interacting finite-state machines.
The program consists of four modules:
 @itemlist[
   @item{
     @tt{automata} defines a state machine datatype and sample implementations;
   } @item{
     @tt{main} is a driver module that creates a population and triggers updates;
   } @item{
     @tt{population} defines a (mutable) datatype for populations;
   } @item{
     and @tt{utilities} provides functions for working with probability vectors.
   }]
All told, there are @exact|{$2^4$}| ways of choosing a subset of these four
 modules to type.
To ground our discussion we focus on the configuration
 where only @tt{population} is typed; specifically, consider the fragments of
 @tt{main} and @tt{population} shown in @Figure-ref{fig:fsm-example}.

@; 1. #lang line
First, note that each module begins with a different @tt{#lang} line.
These directives specify which language---untyped @tt{racket} or @tt{typed/racket}---the
 module is written in.
(The @tt{#lang} lines for @tt{automata} and @tt{utilities} are
 @tt{#lang racket} as well.)
This means that all interactions between @tt{population} and any another
 module go from typed to untyped code.
We express this (symmetric) relationship by saying there is a
 @emph{type boundary} between @tt{population} and the other modules.

@; 2. smooth interaction with require & require/typed
@; 3. opaque types, imported functions
Case in point, the @tt{require/typed} statements in @tt{population}
 mark an explicit boundary to untyped code.
These statements assign types to identifiers crossing a type boundary.
At runtime, values crossing the boundary are validated against the types by a
 runtime assertion or proxy.
Line 4 bears special mention.
It uses an untyped predicate @tt{automaton?} to define a new type, @tt{Automaton},
 to be used when type checking @tt{population}.
If the module ever receives an untyped value where it expects an
 @tt{Automaton} at runtime, it will apply this predicate to decide whether
 to raise a dynamic type error.
This happens, for example, before calling @tt{payoff} with an untyped argument.

@; 4. provided types, require in untyped
Line 4 of @tt{main} also defines a type boundary.
This case is more subtle.
As @tt{main} is untyped, it is not required to annotate values
 imported from @tt{population}.
Instead, the typed module @tt{population} defines a protected set of
 exports for untyped modules to use and shares these through the @racket[require]
 statement.
Any constant values pass through this boundary unchecked, and mutable or
 higher-order values wrap in a proxy that validates runtime input from untyped
 clients.

@;@; 5. type->contract
@;To be precise, each typed module defines two sets of identifiers:
@; unguarded identifiers for use in other typed modules and contract-protected
@; identifiers provided to untyped clients.
@;The compiler decides which set of identifiers to import based on the language
@; of the module currently being compied.
@;Likewise, the values imported through a @tt{require/typed} are wrapped in
@; a contract to ensure they return a well-typed result.

@; 6. step/evolve
Proxied values imported from @tt{population} are used in lines 13 and 16 of
 @tt{main}.
Each call to these functions crosses a type boundary and therefore triggers
 a runtime check.
In the case of @tt{step} (line 13), two checks happen.
@itemlist[
  @item{Before the function call, the program asserts that @tt{pop} has type
     @tt{Population}; that is, @tt{pop} is a vector of vectors of values that
     pass the @tt{automata?} predicate. This check traverses the entire vector.}
  @item{After the function call, the result is proxied to protect its type.
     To be precise, the proxy monitors every future assignment to the vector
     and rejects type-changing updates.}
]
When @tt{step} is called a second time with the result of the first call,
 it performs these checks again, validating and re-wrapping the proxied vector.
Every successive call adds another proxy; each proxy slows down vector reference
 and update operations.

@; So how is performance?
Unsurprisingly, these repeated wraps have an enormous performance impact.
On Racket version 6.2, the program runs in @todo{180} milliseconds with
 no type annotations.
Adding types to @tt{population} as we have done and leaving all other 
 modules untyped increases the running time to @todo{26 minutes---an 8,500x slowdown}.

If, however, we perservere and add types to every module in the program,
 the running time improves to @todo{85} milliseconds.
This net improvement is due to Typed Racket's optimizer, which specializes
 arithmetic and vector operations in typed code.
Without dynamic checks to shadow these optimizations, the program reflects well
 on Typed Racket.

