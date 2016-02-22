#lang scribble/base

@; TODO more story, more wandering in the midst of the lattice, more anti-paths

@require["common.rkt"]
@(require racket/file benchmark-util/data-lattice)

@title[#:tag "sec:story"]{Gradual Typing in Typed Racket}

Our research is motivated by practical experience with Typed Racket,
 the gradually-typed sister language of Racket @todo{cite}.
Developed since 2006, Typed Racket is the oldest and most mature implementation
 of @emph{sound, macro} gradual typing.

Typed Racket's users have built diverse applications:
 plotting libraries,
 web servers,@note{https://groups.google.com/forum/#!searchin/racket-users/carmack/racket-users/RFlh0o6l3Ls/gMbszBQjijsJ}@note{https://twitter.com/ID_AA_Carmack/status/695702976745381889}@note{https://groups.google.com/forum/#!searchin/racket-users/typed$20racket%7Csort:date/racket-users/rfM6koVbOS8/JHaHG03cCQAJ}
 probabilistic programming languages, @; @todo{cite drbayes},
 and music studios.@note{https://con.racket-lang.org/2015/burns.pdf}
They have used Typed Racket both to improve existing untyped code and to
 write new applications from scratch @todo{cite}.

Almost universally, Typed Racket programmers complain about performance.
Slow compile times are one issue, but the cost of interaction with untyped code
 is often a deal-breaker.
Recent complaints include a 50% slowdown from mixing typed and untyped code
 in a commercial web server@note{https://groups.google.com/forum/#!searchin/racket-users/typed$20racket%7Csort:date/racket-users/rfM6koVbOS8/JHaHG03cCQAJ},
 a 10x slowdown after fully typing a program in the hope of improving performance,@note{Personal communication from the author of the @tt{quad} benchmark}
 and a performance degredation from 1 millisecond to @emph{12 seconds} when
 calling a typed data structures library from untyped Racket.@note{https://groups.google.com/forum/#!searchin/racket-users/warning$20on$20using$20trie$20functions/racket-users/WBPCsdae5fs/XDgOdIZlAAAJ}
Typed Racket's math library even includes a disclaimer about 25x-50x slowdowns
 in untyped programs..@note{http://docs.racket-lang.org/math/array.html}

@; Move all the footnotes to an appendix?


@section{Why Gradual Typing?}

If all our users' code was fully typed, these performance
 issues would disappear.
Likewise if all code was untyped, though removing the types would also
 remove the soundness guarantees.
So why bother with gradual typing?
Surely the convenience of switching between static and dynamic type checking
 at a whim is not worth the performance overhead or language design challenges.
To this we have two replies: one practical, one theoretical.


@subsection{Pragmatics of Gradual Typing}
Converting a large, untyped software system to a statically typed
 language---even a gradually typed variant of the original language---is
 a massive undertaking.
The team assigned this task will need intimate knowledge of both the
 target type system and the inner workings of the existing software.
Determining module dependencies and annotating API functions may be straightforward,
 but recovering types of obscure or legacy functions written by a developer
 who has long since left the project is significantly more difficult.

Worse, there is no clear payoff for converting every last bit of untyped functionality.
The program already worked ``correctly'' before conversion began,
 so type checking is unlikely to reveal deep bugs.
This low payoff is underscored by the opportunity cost of migrating existing
 software; the same time could be spent building new features.
@; Y > X but we stay

Instead of requiring a full conversion, gradual typing makes it possible
 to typecheck only core or mission-critical parts of an application.
The converted modules fit into the software system exactly as before,
 but now strengthened with static type checking and dynamic enforcement of type
 signatures.
In short, we gain increased protection from bugs in important parts of the
 codebase and leave the rest alone---without compomising the safety of
 the now-typed parts.

Other components of a system may be converted when, if ever, doing so becomes expedient.
Imagine, for example, that a bug is traced to an untyped module.
The first step in fixing the bug is understanding how that untyped module
 works, typically by reading the code, looking at test cases, and asking other
 developers.
Thus a side effect of debugging untyped code is recovering its type information.
In a gradual type system, the developer can leave these types in the code
 so that after the buggy module is fixed, type annotations harden it against
 future bugs and regressions.

Ultimately, the stable point for a large project initally written in a
 dynamic language is somewhere between fully-typed and fully-untyped,
 where the essential services are statically typed and all other code
 may or may not be typed.
As the program evolves, some new untyped components will appear.
So too will new typed components, and if the type system grows to add
 a new feature then parts of the project may opt-in to the feature without
 forcing a rewrite of neighboring components.
All this is to say that gradual typing is not about making it easy to flip
 a switch from untyped to typed, but rather to turn the decision of whether
 to switch a local choice instead of a global one.


@subsection{Beyond Typed Racket}
Gradual typing, in our mind, is not just about mixing typed and untyped code.
The deeper question is about language interoperability, especially between
 languages that offer different correctness guarantees.
Typed Racket is just one instance, where the type system's guarantees
 are at odds with the freedom of the untyped programming language.
A similar problem concerns typed languages interaction with their untyped
 runtime system, or any interaction between languages through an FFI.

How can we ensure safety when data flows across a boundary?
To date, the solution has been ``by assumption'': we assert that the boundary is correct.
For a runtime system, this approach is justifiable.
Checking every interaction between the runtime and the language would
 incur a large overhead, and besides the runtime is small enough to
 consider it a trusted computing base.
Assuming safety is less valid, however, when reasoning about FFI calls.
It is usually not reasonable to trust that every foreign function is correct.
And in the case of gradual typing, the untyped code is part of the very system
 we hope to debug and maintain by adding type safety.

Thus we think of gradual typing as one motivating example for research on
 language interoperability.
The question is how we can enforce and streamline the conditions necessary
 for correctness.
Once we do find successful techniques, we expect to apply them in other
 multi-language systems.

As a concrete example, the verification community is currently trying to
 bring proof assistants into common use.
This is happening gradually, as high-profile software like C compilers,
 device drivers, OS kernels, and even a web browser shim are implemented in Coq.
Each conversion requires tremendous effort, so despite these successes we
 may never reach the point where Coq is the @emph{lingua franca} for verified
 software.
Therefore we need safe interoperability between verified software and the existing
 code that lives around it.
Until now, interactions have been dealt with case-by-case, but we hope for
 a unified theory and implementation.

@; Snarky aside: GT self-propogating
@; math library etc are forceful models


@section[#:tag "sec:fsm"]{Why is Typed Racket Slow?}

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
 where only @tt{population} is typed; specifically the fragments of
 @tt{main} and @tt{population} shown in @Figure-ref{fig:fsm-example}.

@figure["fig:fsm-example" "Example typed/untyped interaction"
@exact|{\input{fig-fsm-example}}|
]

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
Without dynamic checks to shadow these optimizations, the program makes
 Typed Racket look very good.

