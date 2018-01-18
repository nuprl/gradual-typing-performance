#lang scribble/base

@; TODO first draft, then fuck with wording
@; TODO remove the fucking "SUCH THINGS" shit

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
The dynamically-typed module on the top left implements a guessing game with the function @racket[play].
Each call to @racket[play] generates a random number and returns a function that checks a given number against this chosen number.
The Racket module on the top right implements a @exact|{{na\"ive}}| player.
The driver module at the bottom combines the game and player.
It generates a game, prompts @racket[stubborn-player] for ten guesses, and counts the number of correct guesses using the @racket[for/sum] combinator.
Of the three modules, only the driver is implemented in Typed Racket.
This means that Typed Racket statically checks the body of the driver module under the assumption that its annotations for the @racket[play] and @racket[stubborn-player] functions are correct.  Typed Racket protects this assumption by compiling the types for these functions into contracts; at runtime, the contracts enforce the types.
For example, one contract ensures that @racket[(play)] returns a function from natural numbers to booleans (by applying a new contract to the returned value), and the other ensures that @racket[stubborn-player] returns natural numbers.


@; -----------------------------------------------------------------------------
@section{How Types Spread}

The close integration of Racket and Typed Racket makes gradual typing useful for refactoring existing programs.
By converting a module to Typed Racket, the maintainer receives:
@itemlist[
  @item{
    @emph{assurance} from the typechecker against common bugs;
  }
  @item{
    @emph{documentation} in the form of type annotations;
  }
  @item{
    @emph{protection} against dynamically-typed clients; and
  }
  @item{
    @emph{speed}, when the compiler can use types to generate efficient bytecode.
  }
]
These perceived benefits draw Racket programmers towards Typed Racket.

Another, more subtle, way that Racket users begin using Typed Racket is by importing definitions from a typed library.
For example, every program that uses the built-in @racket[plot] library (including the program that generated this paper) interacts with typed code.

Conversely, Typed Racket programmers may use Racket to work with legacy code, prototype new designs, or write program-manipulating programs.
These are situations where the effort of managing type annotations outweighs the ease of running the Racket code and checking the result.

In summary, Racket and Typed Racket complement one another.
Programs that use both languages are increasingly common, and this situation seems unlikely to change.


@; -----------------------------------------------------------------------------
@section{The Need for Performance Evaluation}

Due to the close integration of Racket and Typed Racket, programmers frequently combine both languages in an application.
These hybrid programs occasionally run much slower than the programmer expects.
As concrete examples, Typed Racket users have reported:
 a 1.5x slowdown in a web server,
 25x--50x slowdowns when using an array library,
 and an over 1000x slowdown when using a library of functional data structures.@note{The appendix contains a list of user reports.}
These reports suggest that Typed Racket has a performance problem, but it is unclear how widespread the problem is.




Reports of poor performance clearly indicate a problem, but it 

These reports clearly indicate that poor performance is the symptom of some problem,
It is unclear, 


@;Oe these three costs, the performance overhead is the most troublesome.
@;One part of the issue is that the magnitude of the cost is difficult to predict.
@;Adding types to one small module can lead to a large performance overhead if the module frequently receives untyped data at runtime.
@;A second part is that experience with statically typed languages---or optionally-typed languages such as TypeScript---teaches programmers that type annotations do not impose runtime overhead.
@;This overhead is crucial, however, to guarantee type soundness in programs that mix typed and untyped code.
@;
@;
@;@;;; Regardless of why such mixes of typed and untyped code arise, the lesson for implementors is that they @emph{do} arise as the solution to practical issues and ought to be supported.
@;@;;; We cannot expect programmers to go fully typed or fully untyped.
@;
@;
@;@; -----------------------------------------------------------------------------
@;@section{The Need for Systematic Performance Evaluation}
@;
@;Since typed/untyped interaction is common and sound typed/untyped interaction
@; adds performance overhead, then the obvious question is: why enforce type soundness?
@;The reason is because errors matter.
@;If an ill-typed value flows into typed code, soundness in Typed Racket guarantees
@; that the program will halt with an informative error message.
@;This property is crucial for maintaining a large application.
@;Otherwise, the program can raise a misleading error message or silently compute a nonsensical result.
@;
@;To demonstrate the kinds of errors that can occur in an unsound gradually-typed language, @figure-ref["fig:voting-machine"] defines an simple API for a voting machine.
@;The function @racket[add-votes!] expects a natural number and adds this number to a running total.
@;
@;In Typed Racket, the compiled version of @figure-ref{fig:voting-machine} differs in two ways from the figure.
@;First, the @racket[provide] statement exports a proxy rather than the unprotected function.
@;Second, the function @racket[+] is replaced with an addition function specialized to natural numbers.
@;Consequently, the call @racket[(add-votes! 1)] runs efficiently and the call @racket[(add-votes! "NaN")] raises an error immediately.
@;
@;In a language that erases types and does not optimize the @racket[+] function, such as TypeScript@~cite[bat-ecoop-2014],
@; the call @racket[(add-votes! "NaN")] does not error immediately.
@;Instead, the function invokes @racket[+] with a string argument; this call to @racket[+] raises an error.
@;The similar call @racket[(add-votes! -1)] silently decrements the total number of votes.
@;
@;A language that erases types and replaces the @racket[+] function with an optimized version is worst off.
@;Depending on the addition function, calls like @racket[(add-votes! "NaN")] and @racket[(add-votes! add-votes!)] can all increment the total by a nonsensical value.
@;This can also happen if a programmer manually replaces the @racket[+] function based on the static types.
@;
@;Type soundness eliminates the risk of such errors.
@;As such, it is the only reasonable default for a gradually-typed language.
@;Worth some performance cost, especially if type-based optimizations can recover some of the overhead.
@;
@;@;Furthermore, even if a dynamic tag check uncovers a logical type error, debugging such errors in a higher-order functional language is often difficult.
@;@;Well-trained functional programmers follow John Hughes' advice and compose many small, re-usable functions to build a program@~cite[h-cj-1989].
@;@;Unfortunately, this means the root cause of a runtime exception is usually far removed from the point of logical failure.
@;@;@; Laziness does not pay in the context of type checking.
@;
@;    @figure["fig:voting-machine" @elem{Voting machine}
@;      @codeblock-pict[@string-join['(
@;        "#lang typed/racket"
@;        "(provide add-votes)"
@;        ""
@;        "(define total-votes : Natural 0)"
@;        ""
@;        "(: add-votes (Natural -> Void))"
@;        "(define (add-votes n)"
@;        "  (set! total-votes (+ total-votes n)))"
@;      ) "\n"]]
@;    ]
@;
@;
