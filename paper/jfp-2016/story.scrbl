#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:story"]{Our Story / Programming in Typed Racket}

Our research is motivated by practical experience with Typed Racket,
 the gradually-typed sister language of Racket @todo{cite}.
Developed since 2006, Typed Racket is the oldest and most mature implementation
 of @emph{sound, macro} gradual typing.
Typed Racket's broad design goals are:
  @itemlist[
    @item{Type Safety}
    @item{Accomodate untyped idioms}
    @item{Encourage typed / untyped programs.
          (Debug 1 module at a time.)}
    @item{Optimize types for performance, where possible}
  ]

Typed Racket's users have built
 graphing libraries,
 web servers,@note{https://groups.google.com/forum/#!searchin/racket-users/carmack/racket-users/RFlh0o6l3Ls/gMbszBQjijsJ}@note{https://twitter.com/ID_AA_Carmack/status/695702976745381889}@note{https://groups.google.com/forum/#!searchin/racket-users/typed$20racket%7Csort:date/racket-users/rfM6koVbOS8/JHaHG03cCQAJ}
 probabilistic programming languages @todo{cite drbayes},
 and music studios.@note{https://con.racket-lang.org/2015/burns.pdf}
They have used Typed Racket both to improve existing untyped code and to
 write new applications from scratch @todo{cite}.

Almost universally, Typed Racket programmers complain about performance.
Slow compile times are one issue, but interaction with untyped code is often a
 deal-breaker.
Recent complaints include a 50% slowdown from mixing typed and untyped code
 in a commercial web server@note{https://groups.google.com/forum/#!searchin/racket-users/typed$20racket%7Csort:date/racket-users/rfM6koVbOS8/JHaHG03cCQAJ},
 a 10x slowdown after fully typing a program in the hope of improving performance,@note{Personal communication from the author of the @tt{quad} benchmark}
 and a performance degredation from 1 millisecond to 12 @emph{seconds} when
 calling a typed data structures library from untyped Racket.@note{https://groups.google.com/forum/#!searchin/racket-users/warning$20on$20using$20trie$20functions/racket-users/WBPCsdae5fs/XDgOdIZlAAAJ}
Typed Racket's math library even includes a disclaimer about 25x-50x slowdowns.@note{http://docs.racket-lang.org/math/array.html}

If all our users' code was fully typed, these performance
 issues would likely disappear.
But then so would the benefits of combining typed and untyped code,
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



@section{Typed Racket Internals}

Having established that Typed Racket @emph{has} performance issues, we discuss
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
@; 2. smooth interaction with require & require/typed
@; 3. opaque types, imported functions
@; 4. provided types, require in untyped
@; 5. type->contract
@; 6. evaluation proceeds


@;Worse yet, these slowdowns are often unpredictable.
@;Library users have no uniform way of knowing whether they are interacting with
@; typed or untyped code.
@;Maintainers of large systems rarely have the 
@;
@;One promise of Typed Racket's gradual typing is that new typed products can be
@; built on top of existing untyped code @emph{without modifying} the untyped
@; code.
@;TODO the debug story

@; We need _language designer_ tools to evaluate performance
@; so we can tell where to improve!
