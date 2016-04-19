@; LOOKING FOR A HOME

@; =============================================================================
@; =============================================================================
@; =============================================================================
@; =============================================================================

@; -----------------------------------------------------------------------------
@section{Lattice-Based Evaluation}
@todo{want this section?}

@Secref{sec:fsm} described a 4-module benchmark, @bm{fsm}, and remarked that
 although the fully-typed configuration ran faster than the untyped program,
 a configuration with one typed module experienced an @todo{8,500x} slowdown.
The modules in @bm{fsm} were named @tt{automata}, @tt{main},
 @tt{population}, and @tt{utilities};
 the above-noted slow configuration assigned types only in @tt{population}.
Henceforth, we will represent configurations of @bm{fsm} as 4-bit binary
 strings corresponding to the module names in alphabetic order.
Using this notation, the configuration where only @tt{population} typed
 has the bitstring @tt{0010}.

@figure*["fig:fsm-lattice-6.2"
  @list{FSM data lattice}
  @(render-data-lattice 'fsm "6.2" #:tag "2016-03-19T02:06:11")
]

@Figure-ref{fig:fsm-lattice-6.2} is the full performance lattice for @bm{fsm}
 run on Racket version 6.2.
Each configuration is represented by a sequence of colored shapes,
 corresponding to its bitstring.
A black shape represents a typed module and a white shape is an untyped one;
 the shape at index @math{i} from the left is colored
 iff the bit at index @math{i} from the left is 1,
 meaning the @tt{automata} module is typed.
Nodes are labeled with the configuration's overhead---computed
 as the configuration's mean runtime divided by the fully-untyped
 configuration's mean runtime---and
 the standard error of our timings for that configuration.
Configurations @tt{0010}, and @todo{others} suffer from a boundary between
 @tt{main} and @tt{population}.
These are by far the slowest configurations and no path through the lattice
 can avoid all of them.
But the other 8 configurations are at worst slightly slower than untyped,
 and two of these improve on the baseline performance.


@; -----------------------------------------------------------------------------
@; @subsubsection{Comparing Lattices}

Given that only half of @bm{fsm}'s configurations are slow, and that furthermore
 the slow configurations can be avoided by adding types to either @tt{main}
 (~25 lines) or @tt{population} (~50 lines), the @tt{fsm} benchmark suggests
 that Typed Racket's gradual typing is performant as of the v6.2 release.@note{
   Besides, of course, the overwhelming cost of repeatedly wrapping a vector.}
There are two ways to further validate the performance of Typed Racket:
 by comparing with other gradual type systems and by testing more programs (@Secref{sec:lnm}).

While not technically a competing implementation of Typed Racket v6.2,@note{
   Pycket @todo{cite} is a competing implementation of gradual typing for Typed Racket.
   At the time of writing Pycket could not run all our benchmark programs, but
    the Pycket authors will soon publish their own analysis.}
 we can compare our lattice against results for versions 6.3 and 6.4.
These lattices are shown in @Figure-ref{fig:fsm-lattice-6.3}.

@todo{describe}

@figure*["fig:fsm-lattice-6.3"
  @list{Annotated performance lattice for @bm{fsm} v6.2}
  @todo{(hc-append 8 ...)}
  @todo{(data-lattice 'fsm "6.3")}
  @todo{(data-lattice 'fsm "6.4.0.5")}
]

This is a sad story for Typed Racket, but we promise to improve for version 6.5.
@todo{say more}


@; -----------------------------------------------------------------------------
@subsection{Limitations of Lattice-Based Evaluation}

Inspecting the annotated performance lattice for @bm{fsm} is feasible and
 even gives insight as to why the worst configurations are slow.
At a glance, it is fairly easy to see that the 8 slow modules match the pattern
 @todo{pattern},
 @;@tt{*01*} or @tt{*10*},
 corresponding to a type boundary between @tt{main}
 and @tt{population}. @todo{use shapes?}
The number of nodes in a lattice, however, is exponential in the number of
 modules in a program.
Visual inspection quickly becomes impossible.

@todo{FILL IN HERE}


@; =============================================================================
@; =============================================================================
@; =============================================================================
@; =============================================================================

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

