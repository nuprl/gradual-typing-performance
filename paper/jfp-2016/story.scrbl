#lang scribble/base

@require["common.rkt" "benchmark.rkt" "util.rkt" racket/file
         (only-in pict vc-append rt-superimpose hline ht-append pict-width pict-height frame text vline)
         (only-in pict/code codeblock-pict) (only-in racket/string string-join)]

@profile-point{sec:story}
@title[#:tag "sec:story"]{Gradual Typing in Typed Racket}

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

Another, more subtle, way that Racket users start using Typed Racket is by importing definitions from a typed library.
For example, every program that uses the built-in @racket[plot] library (including the program that generated this paper) interacts with typed code.

Conversely, Typed Racket programmers may use Racket to work with legacy code, prototype new designs, or write program-manipulating programs.
These are situations where the effort of managing type annotations outweighs the ease of running the Racket code and checking the result.

In summary, Racket and Typed Racket complement one another.
Programs that use both languages are increasingly common, and this situation seems unlikely to change.


@; -----------------------------------------------------------------------------
@section{The Need for Performance Evaluation}

Typed Racket is an evolving research project.
The initial release delivered a sound type system that could express the idioms in simple Racket programs.
Subsequent improvements focused on growing the type system and making it easier to use.
All along, the maintainers of Typed Racket knew that the performance cost of enforcing type soundness could be high (see @secref{sec:devils} for an overview); however, this cost was not an issue in many programs.

As other programmers began using Typed Racket, a few discovered serious issues with its performance.
Some users were able to work around their issues, but for others the poor performance became an obstacle to their work.
For instance, one user experienced a 1.5x slowdown in a web server,
 others found 25x--50x slowdowns when using an array library,
 and two others reported over 1000x slowdowns when using a library of functional data structures.@note{The appendix contains a list of user reports.}
These reports identified the need for a systematic performance evaluation.

The goals of our performance evaluation are to:
 (1) determine the extent of the issue,
 (2) identify the sources of poor performance,
 (3) help users avoid performance issues, and
 (4) improve the performance of Typed Racket.
The evaluation method in @secref{sec:method} has been effective for measuring the problem@~cite[tfgnvf-popl-2016], and this paper demonstrates that the same method can quantify improvements to Typed Racket that have appeared since then.
This paper also discusses the source of poor performance.


