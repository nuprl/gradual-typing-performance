#lang scribble/base

@require["common.rkt" "benchmark.rkt" "util.rkt" racket/file
         (only-in pict vc-append rt-superimpose hline ht-append pict-width pict-height frame text vline)
         (only-in pict/code codeblock-pict) (only-in racket/string string-join)]

@profile-point{sec:story}
@title[#:tag "sec:story"]{Gradual Typing in Typed Racket}

@; ROUGH OUTLINE
@; * Gradual Typing in Typed Racket
@;   - TR is a sound gradual typing system for Racket
@;   - originally from Sam + Matthias 2006 2008, grown to GitHub contributors
@;   - Racket idioms
@;   - type sound via contracts
@;   - (type-based optimizations)
@;   - Figure gives example program
@; * Obvious Benefits of Migration
@;   - static checks
@;   - soundness => trustworthy documentation
@;   - optimizer
@;   - great to not convert other modules, saves annotation + refactoring effort
@;   - conclusion, mixed programs is normal
@;   - ALSO library users have no control over their favorite library's language
@; * Less-Obvious Costs
@;   - over time users discovered programs with abysmal performance
@;   - we nkew this _could_ happen, quick examples of natural embedding
@;   - didn't realize would be issue in practice until it was
@;   - clearly need systematic evaluation
@;   - hence this paper, and hence the coming section

Typed Racket@~cite[tfffgksst-snapl-2017] is a sound gradual typing system for Racket.
Consequently, Typed Racket's syntax is an extension of Racket's, its static type checker supports idioms common in (dynamically-typed) Racket programs@~cite[thf-popl-2008 stf-esop-2009 tfdffthf-ecoop-2015], and a Typed Racket module may import definitions from a Racket module.
A Racket module may likewise import definitions from a Typed Racket module.
To ensure type soundness@~cite[tfffgksst-snapl-2017], Typed Racket compiles static types to higher-order contracts and applies these contracts at the boundaries between Typed Racket and Racket modules.

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
Of the three modules, only the driver is implemented in Typed Racket.
This means that Typed Racket statically checks the body of the driver module under the assumption that its annotations for the @racket[play] and @racket[stubborn-player] functions are correct.  Typed Racket protects this assumption by compiling the types for these functions into contracts; at runtime, the contracts enforce the types.
For example, one contract ensures that @racket[(play)] returns a function from natural numbers to booleans (by applying a new contract to the returned value), and the other ensures that @racket[stubborn-player] returns natural numbers.


@; -----------------------------------------------------------------------------
@section{The Obvious Benefits of Sound Gradual Typing}

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
Without soundness, the types are statically-checked suggestions about what the program ought to do; in particular, unsound types give no assurance against runtime errors and cannot be trusted by a compiler.

@;@note{Ren and Foster recently applied just-in-time type checking to six Ruby programs and found @emph{zero} latent type errors.}

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
@section{The Unexpectedly Large Performance Cost}

Since typed/untyped interaction is common and sound typed/untyped interaction
 adds performance overhead, then the obvious question is: why enforce type soundness?
The reason is because errors matter.
If an ill-typed value flows into typed code, soundness in Typed Racket guarantees
 that the program will halt with an informative error message.
This property is crucial for maintaining a large application.
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
As such, it is the only reasonable default for a gradually-typed language.
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


