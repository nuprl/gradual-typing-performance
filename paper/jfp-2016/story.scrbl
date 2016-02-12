#lang scribble/base

@; TODO more story, more wandering in the midst of the lattice, more anti-paths

@require["common.rkt"]
@(require racket/file benchmark-util/data-lattice)

@title[#:tag "sec:story"]{Our Story / Programming in Typed Racket}

Our research is motivated by practical experience with Typed Racket,
 the gradually-typed sister language of Racket @todo{cite}.
Developed since 2006, Typed Racket is the oldest and most mature implementation
 of @emph{sound, macro} gradual typing.

Typed Racket's users have built
 graphing libraries,
 web servers,@note{https://groups.google.com/forum/#!searchin/racket-users/carmack/racket-users/RFlh0o6l3Ls/gMbszBQjijsJ}@note{https://twitter.com/ID_AA_Carmack/status/695702976745381889}@note{https://groups.google.com/forum/#!searchin/racket-users/typed$20racket%7Csort:date/racket-users/rfM6koVbOS8/JHaHG03cCQAJ}
 probabilistic programming languages @todo{cite drbayes},
 and music studios.@note{https://con.racket-lang.org/2015/burns.pdf}
They have used Typed Racket both to improve existing untyped code and to
 write new applications from scratch @todo{cite}.

Almost universally, Typed Racket programmers complain about performance.
Slow compile times are one issue, but the cost of interaction with untyped code
 is often a deal-breaker.
Recent complaints include a 50% slowdown from mixing typed and untyped code
 in a commercial web server@note{https://groups.google.com/forum/#!searchin/racket-users/typed$20racket%7Csort:date/racket-users/rfM6koVbOS8/JHaHG03cCQAJ},
 a 10x slowdown after fully typing a program in the hope of improving performance,@note{Personal communication from the author of the @tt{quad} benchmark}
 and a performance degredation from 1 millisecond to 12 @emph{seconds} when
 calling a typed data structures library from untyped Racket.@note{https://groups.google.com/forum/#!searchin/racket-users/warning$20on$20using$20trie$20functions/racket-users/WBPCsdae5fs/XDgOdIZlAAAJ}
Typed Racket's math library even includes a disclaimer about 25x-50x slowdowns.@note{http://docs.racket-lang.org/math/array.html}


@section{Why Gradual Typing?}

@; TODO
@; - can't deprecate racket & go fully-typed
@; - much more untyped racket than typed racket

If all our users' code was fully typed, these performance
 issues would likely disappear.
But then so would the benefits of combining typed and untyped code,
 @; It's NOT "not to mention", I definitely want to mention this.
 not to mention the tremendous engineering cost of coverting millions of
 lines of useful, existing Racket code.
In fact, some untyped code cannot faithfully be expressed in Typed Racket
 due to limitations in the type system.
Lastly, what should happen when dependently typed Racket arrives in
 20XX---can we afford a second great migration?

Thus we conclude that the Typed Racket language has implementation
 issues that must be addressed.
Once the worst slowdowns are accounted for, we need to shift focus to building
 tools for avoiding performance traps.

@; TODO not about paths!
@; TODO can't predict where you'll end up


@section{Typed Racket, in depth}

Having established that Typed Racket has performance issues, we discuss
 exactly @emph{why} these issues come about.
The purpose of this section is to explain relevant technical details of
 Typed Racket's implementation and to motivate the evaluation framework
 presented in @Secref{sec:framework}.

Our running example for this section is a small program, @tt{fsm},
 that simulates an economy of interacting finite-state machines.
The program consists of four modules:
 @tt{automata.rkt} defines a state machine datatype and sample implementations;
 @tt{main.rkt} is a driver module that creates and evolves an economy;
 @tt{population.rkt} defines a datatype for populations, or economies;
 and @tt{utilities.rkt} provides functions for working with probability vectors.

@figure["fig:fsm-example" "Example typed/untyped interaction"
@exact|{\input{fig-fsm-example}}|
]

All told, there are @exact|{$2^4 = 16$}| ways of choosing a subset of these four
 modules to type, but to ground our discussion we focus on the configuration
 where only @tt{population.rkt} is typed.
To illustrate exactly how boundaries can occur in a Typed Racket program,
 we focus on the fragments of @tt{main.rkt} and @tt{population.rkt} shown
 in figure @Figure-ref{fig:fsm-example}.

@; 1. #lang line
First, note that each module begins with a different @tt{#lang} line.
These directives specify which language---untyped @tt{racket} or @tt{typed/racket}---the
 module is written in.
The @tt{#lang} lines for @tt{automata.rkt} and @tt{utilities.rkt} are
 @tt{#lang racket} as well.
This means that all interactions between @tt{population.rkt} and any another
 module go from typed to untyped code.
We express this (symmetric) relationship by saying there is a
 @emph{type boundary} between @tt{population.rkt} and the other modules.

@; 2. smooth interaction with require & require/typed
@; 3. opaque types, imported functions
Case in point, the @tt{require/typed} statements in @tt{population.rkt}
 mark an explicit boundary to untyped code.
These statements assign types to identifiers imported from untyped modules.
Henceforth, the module is typechecked using the supplied type annotations.
Line 4 is slightly different; it uses an untyped predicate @tt{automaton?} to define
 a new type, @tt{Automaton}.
If @tt{population.rkt} receives an untyped value where it expects an
 @tt{Automaton}, it will apply this predicate to decide whether to raise a
 type error.
This happens, for example, before calling @tt{payoff} with an untyped argument.

@; 4. provided types, require in untyped
Line 4 of @tt{main.rkt} also defines a type boundary.
This case is more subtle.
As @tt{main.rkt} is untyped, it is not required to annotate the values
 @tt{create} and @tt{step} imported from @tt{population.rkt}.
Instead, the final two lines of @tt{population.rkt} declare the identifiers'
 export type.
Inside @tt{main.rkt}, these types are compiled to function contracts asserting
 that each call to @tt{create} or @tt{step} is made with well-typed arguments.
The translation is invisible to the user because the contracts are determined
 by the static types.

@; 5. type->contract
To be precise, each typed module defines two sets of identifiers:
 unguarded identifiers for use in other typed modules and contract-protected
 identifiers provided to untyped clients.
The compiler decides which set of identifiers to import based on the language
 of the module currently being compied.
Likewise, the values imported through a @tt{require/typed} are wrapped in
 a contract to ensure they return a well-typed result.

@; 6. step/evolve
The last notable boundaries in our example occur at lines 13 and 16 of
 @tt{main.rkt}.
Here the untyped module calls typed functions from @tt{population.rkt}.
Each of these calls crosses a type boundary and therefore triggers
 a runtime check.
In the case of @tt{step} (line 13), two checks happen.
@itemlist[
  @item{Before the function call, the program asserts that @tt{pop} has type
     @tt{Population}; that is, @tt{pop} is a vector of vectors of values that
     pass the @tt{automata?} predicate. This check traverses the entire vector.}
  @item{After the function call, the result is proxied to prevent untyped code
     from changing an element of the @tt{Population} vector to a value without
     the @tt{Automata} type.}
]
When @tt{step} is called a second time with the result of the first call,
 it performs these steps again, validating and wrapping the proxied vector.
Every successive call adds another proxy; each proxy slows down vector reference
 and update operations.

@; So how is performance?
Unsurprisingly, these repeated wraps have an enormous performance impact.
On Racket version 6.3, the program runs in 180 milliseconds with no type annotations.
Adding types to @tt{population.rkt} as we have done increases the running time
 to 26 minutes---an 8,500x slowdown.

If, however, we perservere and add types to every module in the program,
 the running time improves to 85 milliseconds.
This net improvement is due to Typed Racket's optimizer, which specializes
 arithmetic and vector operations in typed code.
Without dynamic checks to shadow these optimizations, the program makes
 Typed Racket look very good.


