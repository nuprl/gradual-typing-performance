#lang scribble/base

@require["common.rkt" "benchmark.rkt" "util.rkt" racket/file
         (only-in pict vc-append rt-superimpose hline ht-append pict-width pict-height frame text vline)
         (only-in pict/code codeblock-pict) (only-in racket/string string-join)]

@profile-point{sec:story}
@title[#:tag "sec:story"]{Gradual Typing in Typed Racket}

Typed Racket@~cite[thf-popl-2008] is the oldest and most developed implementation of sound gradual typing.
It supports clients in both academia and industry.
Typed Racket attracts these clients because it accomodates the idioms of (untyped) Racket;
 its type system can express concepts such as
 variable-arity polymorphism@~cite[stf-esop-2009],
 first-class classes@~cite[tsdthf-oopsla-2012],
 and delimited continuations@~cite[tsth-esop-2013].
A typed module may incorporate definitions from a Racket module with a type-annotated import statement;
 conversely, a Racket module may use definitions from a Typed Racket module without knowledge that the providing module is typed.

@figure["fig:story:tr" "A gradually typed application"
  @(let* ([add-name (lambda (pict name) (rt-superimpose pict (frame (text (string-append " " name " ") "black" 10))))]
          [c1
             @codeblock-pict[@string-join['(
             "#lang racket"
             ""
             "(provide play)"
             ""
             "(define (play)"
             "  (define n (random 10))"
             "  (λ (guess)"
             "    (= guess n)))"
             ) "\n"]]]
          [c1-name "guess-game"]
          [c1/name (add-name c1 c1-name)]
          [c2
             @codeblock-pict[@string-join['(
             "#lang racket"
             ""
             "(provide stubborn-player)"
             ""
             "(define (stubborn-player i)"
             "  4)"
             ) "\n"]]]
          [c2-name "player"]
          [c2/name (add-name c2 c2-name)]
          [add-rkt (lambda (s) (string-append s ".rkt"))]
          [c3
             @codeblock-pict[@string-join[(list
             "#lang typed/racket"
             ""
             (format "(require/typed ~s" (add-rkt c1-name))
             "  [play (-> (-> Natural Boolean))])"
             (format "(require/typed ~s" (add-rkt c2-name))
             "  [stubborn-player (-> Natural Natural)])"
             ""
             "(define check-guess (play))"
             "(for/sum ([i : Natural (in-range 10)])"
             "  (if (check-guess (stubborn-player i)) 1 0))"
             ) "\n"]]]
          [c3-name "driver"]
          [c3/name (add-name c3 c3-name)]
          [c1+c2
           @ht-append[8 c1/name @vline[2 (+ 2 (max (pict-height c1) (pict-height c2)))] c2/name]]
             )
  @vc-append[8
    c1+c2
    @hline[(pict-width c1+c2) 2]
    c3/name]
  )
]

@Figure-ref{fig:story:tr} demonstrates gradual typing in Typed Racket with a small application.
The untyped module on the top left implements a guessing game with the function @racket[play].
Each call to @racket[play] generates a random number and returns a function that checks a given number against this chosen number.
The untyped module on the top right implements a @exact|{{na\"ive}}| player.
The driver module at the bottom combines the game and player.
It generates a game, prompts @racket[stubborn-player] for ten guesses, and counts the number of correct guesses using the @racket[for/sum] combinator.
Unlike the other two, the driver module is implemented in Typed Racket.
Typed Racket ensures that the game and player follow the type specifications in the driver's @racket[require/typed] clauses.
More precisely, Typed Racket statically checks that the driver module sends only natural numbers to the guessing game and player, and inserts dynamic checks to enforce the return types of @racket[play], @racket[stubborn-player], and @racket[check-guess].

In general, Typed Racket extends Racket with three ingredients:
 syntax for type annotations,
 a static type checker,
 and a compiler that maps a static type @type{$\tau$} to a contract @ctc{$\tau$} that enforces the type.
The rest of this section explores practical aspects of Typed Racket.


@; -----------------------------------------------------------------------------
@section{Benefits of Migratory Typing}

Due to the close integration of Racket and Typed Racket, programmers frequently use both languages within a single application.
Furthermore, programmers often migrate Racket modules to Typed Racket as their application evolves.
Some common motivations are:
@itemlist[
  @item{
    The typechecker provides @emph{assurance} against common bugs.
     @;, for example,
     @;forgetting to check for the end-of-file value when reading from a port.
  }
  @item{
    Type signatures serve as machine-enforced @emph{documentation}.
  }
  @item{
    Typed modules gain @emph{performance} improvements from the Typed Racket compiler.
  }
  @item{
    Adding typed modules reduces @emph{friction} with typed libraries and clients.
     @; optimizer@~cite[stf-optimization-coaching]
     @; and, unlike untyped modules, suffer no dynamic cost if tightly-coupled to other typed modules.
  }
]
Regarding the final point, there are two sources of friction@note{We borrow this use of the word friction from @citet[bbdt-ecoop-2016], though their work is not directly about typed/untyped interaction.} between typed and untyped code.
The first is the above-mentioned requirement that typed clients must supply type annotations to use imports from an untyped library.
Maintainers of such libraries can instead provide a bridge module with the necessary annotations.
The second is the performance overhead of typed/untyped interaction, such as the overhead of dynamically enforcing the return type of @racket[play] in @figure-ref{fig:story:tr}.

Most of the above benefits are made possible by Typed Racket's soundness guarantee@~cite[tfffgksst-snapl-2017].
Soundness intuitively means that every assumption made by the static type checker holds for every execution of a program.
Formally, if @exact{$e$} is an expression of type @type{$\tau$} then running @exact{$e$} will result in one of four possible outcomes:
  the program execution reduces to a value of type @type{$\tau$};
  the execution diverges;
  the execution ends in an error due to a partial primitive operation (e.g. division);
  or the execution ends in a @emph{type boundary error} that points to: a static boundary between typed and untyped code, a type annotation, and a value that does not match@note{A type boundary error occurs when a type annotation imposes a constraint that untyped code does not satisfy.  The error may indicate a latent bug in the untyped code, but it is equally likely that the type annotation is an incorrect specification.  In other words, the slogan @exact{``well-typed programs can't be blamed''}@~cite[wf-esop-2009] misses the point of gradual typing.} the type.

With soundness, programmers can use the types in a program to reason about its runtime behavior.
Without soundness, the types are statically-checked suggestions about what the program ought to do; in particular, unsound types give no assurance against runtime errors and cannot be leveraged by a compiler.

@;@note{Ren and Foster recently applied just-in-time type checking to six Ruby programs and found @emph{zero} latent type errors.}


@; -----------------------------------------------------------------------------
@section{Costs of Migratory Typing}

Despite the benefits, adding types to untyped code is a tradeoff.
In particular, converting an untyped module incurs three immediate engineering costs.
These costs explain why programmers might prefer to work with a partially-typed codebase.

The first cost is the burden of writing and maintaining type annotations.
In particular, Typed Racket is a @emph{macro-level}@exact{\,}@note{As opposed to @emph{micro-level} gradual typing@~cite[svcb-snapl-2015].} gradual type system; every expression in a Typed Racket module must pass the type checker.
Consequently, every recursive type in a module needs a declaration and every function parameter, class field, struct declaration, and induction variable needs a type annotation.

The second cost is the risk of introducing bugs during the conversion.
Typed Racket mitigates this risk by accomodating Racket idioms,
 but occasionally programmers must refactor code to satisfy the type checker.
Refactoring can always introduce bugs.

The third cost is performance overhead due to typed/untyped interaction.
For example, Typed Racket developers have experienced pathologies including
 a 1.5x overhead in a commercial web server
 and 25x-50x slowdowns when using the (typed) math library.
Another programmer found that converting a script from Racket to Typed Racket improved its performance from @|PFDS-BEFORE| to @|PFDS-AFTER|.@note{The appendix contains a list of user reports.}

Of these three costs, the performance overhead is the most troublesome.
One part of the issue is that the magnitude of the cost is difficult to predict.
Adding types to one small module can lead to a large performance overhead if the module frequently receives untyped data at runtime.
A second part is that experience with statically typed languages---or optionally-typed languages such as TypeScript---teaches programmers that type annotations do not impose runtime overhead.
This overhead is crucial, however, to guarantee type soundness in programs that mix typed and untyped code.


@; -----------------------------------------------------------------------------
@section[]{How Typed Racket Enforces Type Soundness}

When typed code expects a value of type @type{$\tau$} from an untyped source,
 Typed Racket compiles the type @type{$\tau$} to a contract @ctc{$\tau$} that
 enforces the type.
Likewise, when typed code provides a typed value to an untyped context, Typed
 Racket protects the typed value with a contract.
These contracts are the source of performance overhead in partially-typed Typed
 Racket programs; this section briefly describes the nature of this overhead.

The goal of a contract @ctc{$\tau$} is to enforce type soundness.
For any value @${v} such that the application of @ctc{$\tau$} to @${v}
 does not error, the result @ctcapp["\\tau" "v"] must behave like a well-typed
 value.
To a first approximation, one can think of such contracts as dynamic checks;
 however, this reasoning breaks down for mutable values and functional values.
For example, if @type{$\tau$} is a type representing a mutable cell containing an
 integer, then @ctcapp["\\tau" "v"] must (1) check that @${v} currently contains
 an integer and (2) ensure that every read from @${v} in typed code produces an integer.
It does not suffice to check the mutable cell at the boundary because untyped
 code might keep a reference to the cell, and later write an ill-typed
 value to it.

Typed Racket's implementation follows the @emph{natural embedding} strategy
 formalized by @citet[mf-toplas-2007].
In a nutshell, this implementation eagerly checks values when possible and
 otherwise wraps them in a type-checking proxy.
For example:
@itemlist[
@item{
  If @type{$\tau$} is @type{$\tint$} then @ctcapp["\\tau" "v"] checks that @${v} is an integer value.
}
@item{
  If @type{$\tau$} is @type{$(\tlistof{\tint})$} then @ctcapp["\\tau" "v"] checks that @${v}
   is a list and checks @ctc{$\tint$} for every value in the list.
}
@item{
  If @type{$\tau$} is @type{$(\tvectorof{\tint})$} then @ctcapp["\\tau" "v"] checks that @${v}
   is a vector, checks @ctc{$\tint$} for every value in the vector, and wraps @${v}
   in a proxy that checks @ctc{$\tint$} for every read and write to the vector.
  @; need to check reads, because untyped could write directly to `v` (bypassing the proxy)
}
@item{
  If @type{$\tau$} is @type{$(\tarrow{\tint}{\tint})$} then @ctcapp["\\tau" "v"] checks that
   @${v} is a function and wraps @${v} in a proxy that checks @ctc{$\tint$} for
   every argument to the function and every result computed by the function.
}
]
Note that the cost of checking a type like @type{$\tlistof{\tint}$} is linear in the size of the value.
Furthermore, if @ctcapp["\\tau" "v"] wraps @${v} in a proxy then
 (@ctc{$\tau$} @ctcapp["\\tau" "v"]) wraps @${v} in two proxies, and therefore adds two levels of indirection.

@; comment: expensive, but enforces soundness and catches errors ASAP ?


@; -----------------------------------------------------------------------------
@section[#:tag "sec:overhead"]{A Case for Sound Gradual Typing}

Since typed/untyped interaction is common and sound typed/untyped interaction
 adds performance overhead, then the obvious question is: why enforce type soundness?
The reason is because errors matter.
If an ill-typed value flows into typed code, soundness in Typed Racket guarantees
 that the program will halt with an informative error message.
We believe that this property is crucial for maintaining a large application.
Otherwise, the program can raise a misleading error message or silently compute a nonsensical result.

To demonstrate the kinds of errors that can occur in an unsound gradually-typed language, @figure-ref["fig:voting-machine"] defines an simple API for a voting machine.
The function @racket[add-votes!] expects a natural number and adds this number to a running total.

In Typed Racket, the compiled version of @figure-ref{fig:voting-machine} differs in two ways from the figure.
First, the @racket[provide] statement exports a proxy rather than the unprotected function.
Second, the function @racket[+] is replaced with an addition function specialized to natural numbers.
Consequently, the call @racket[(add-votes! 1)] runs efficiently and the call @racket[(add-votes! "NaN")] raises an error immediately.

In a language that erases types and does not optimize the @racket[+] function, such as TypeScript@~cite[bat-ecoop-2014],
 the call @racket[(add-votes! "NaN")] does not error immediately.
Instead, the function invokes @racket[+] with a string argument; this call to @racket[+] raises an error.
The similar call @racket[(add-votes! -1)] silently decrements the total number of votes.

A language that erases types and replaces the @racket[+] function with an optimized version is worst off.
Depending on the addition function, calls like @racket[(add-votes! "NaN")] and @racket[(add-votes! add-votes!)] can all increment the total by a nonsensical value.
This can also happen if a programmer manually replaces the @racket[+] function based on the static types.

Type soundness eliminates the risk of such errors.
As such, we believe it is the only reasonable default for a gradually-typed language.
Worth some performance cost, especially if type-based optimizations can recover some of the overhead.

@;Furthermore, even if a dynamic tag check uncovers a logical type error, debugging such errors in a higher-order functional language is often difficult.
@;Well-trained functional programmers follow John Hughes' advice and compose many small, re-usable functions to build a program@~cite[h-cj-1989].
@;Unfortunately, this means the root cause of a runtime exception is usually far removed from the point of logical failure.
@;@; Laziness does not pay in the context of type checking.

    @figure["fig:voting-machine" @elem{Voting machine}
      @codeblock-pict[@string-join['(
        "#lang typed/racket"
        "(provide add-votes)"
        ""
        "(define total-votes : Natural 0)"
        ""
        "(: add-votes (Natural -> Void))"
        "(define (add-votes n)"
        "  (set! total-votes (+ total-votes n)))"
      ) "\n"]]
    ]


