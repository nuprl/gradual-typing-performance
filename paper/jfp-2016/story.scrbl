#lang scribble/base

@require["common.rkt" "benchmark.rkt" "util.rkt" racket/file]

@profile-point{sec:story}
@title[#:tag "sec:story"]{Gradual Typing in Typed Racket}

Typed Racket@~cite[TypedRacket] is the oldest and most developed implementation of sound gradual typing.
It supports clients in both academia and industry.
Typed Racket attracts these clients because it supports the idioms of Racket.
In fact, a major design goal of Typed Racket is that equipping a Racket program
 with types is only a matter of annotating identifiers and declaring recursive types.
Much of the underlying Racket program can remain the same, including code using
 variable-arity polymorphism@~cite[stf-esop-2009],
 first-class classes@~cite[tsdthf-oopsla-2012],
 delimited continuations@~cite[tsth-esop-2013],
 or contracts@~cite[l-mthesis-2016].
Finally, a typed module may incorporate definitions from a Racket module with a type-annotated import statement;
 conversely, a Racket module may use definitions from a Typed Racket module without knowledge that the providing module is typed.

Due to the close integration of Racket and Typed Racket, programmers frequently use both languages within a single application.
Furthermore, programmers often migrate Racket modules to Typed Racket as their application evolves.
In general one cannot predict why or how such incremental migrations happen, but here are some common motivations.
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
    Adding typed modules improves @emph{compatibility} with typed libraries and clients.
     @; optimizer@~cite[stf-optimization-coaching]
     @; and, unlike untyped modules, suffer no dynamic cost if tightly-coupled to other typed modules.
  }
]
  @;@item{
  @;  Racket libraries maintain an interface of @emph{compatible} type annotations,
  @;   saving typed clients the need to supply their own.
  @;}
  @;@item{
  @;  Programmers convert modules that are @emph{simple} to type.
  @;  These modules could be the smallest, or have the fewest dependencies.
  @;}
Regarding the final point, there are two sources of friction between typed and untyped code.
First is the above-mentioned requirement that typed clients must supply type annotations to use imports from an untyped library.
Maintainers of such libraries can instead provide a bridge module with the necessary annotations.
Second is the performance overhead of typed/untyped interaction.
Suffice to say, an application's performance may improve if the developer adds types to an untyped module that is tightly coupled to other typed modules.

Let us now illustrate one possible evolution.
Imagine a large application in which an error is traced to an untyped module that was written years ago.
In all likelihood, the programmer tasked with fixing the bug must recover the type specifications that the original developer had in mind---but did not write down.
Doing so involves studying the code and analyzing its unit tests.
This is a significant burden, but if the developer converts the
 module to Typed Racket, future maintainers can re-use these specifications.


@; -----------------------------------------------------------------------------
@section{The Costs of Incremental Typing}

Adding types to an untyped module is a tradeoff.
Performing the type conversion may yield long-term benefits, but incurs immediate engineering costs.

The first cost is the burden of writing and maintaining type annotations.
In particular, Typed Racket is a @emph{macro-level}@exact{\,}@note{As opposed to @emph{micro-level}, see @secref{sec:flavors}.} gradual type system.
Every expression within a Typed Racket module must pass the type checker.
Consequently, every recursive type in a module needs a declaration and every function parameter, class field, and induction variable needs a type annotation.

The second cost is the risk of introducing bugs during the conversion.
Typed Racket mitigates this risk by accomodating many Racket idioms,
 but occasionally programmers must refactor code to satisfy the type checker.
Refactoring can always spawn bugs.

The third cost is performance overhead, as mentioned in @secref{sec:intro}.
For example, Typed Racket developers have experienced pathologies including
 a 50% overhead in a commercial web server
 and 25x-50x slowdowns when using the (typed) math library.
Another programmer found that converting a script from Racket to Typed Racket improved its performance from @|PFDS-BEFORE| to @|PFDS-AFTER|.@note{The appendix contains a list of user reports.}

Of these three costs, the performance overhead is the most troublesome.
Part of the issue is that the cost is implicit.
Unlike the tangible cost of
 writing type annotations or the perennial risk of introducing bugs, the
 performance overhead of enforcing type soundness is not apparent until runtime.
Additionally, experience with statically typed languages---or optionally-typed languages such as TypeScript---teaches programmers that type annotations do not impose runtime overhead.
Typed Racket insists on gradual type soundness, therefore types need runtime enforcement.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:overhead"]{A Case for Sound Gradual Typing}

Recall that types are checkable statements about program expressions.
Soundness means that every checked statement holds as the program is executed.
Statically typed languages provide this guarantee by type checking every expression.
In a gradually typed language, the type system cannot check every expression because some are intentionally untyped.
Therefore a sound gradual type system monitors the interaction of typed and untyped program components at runtime.
Where typed code claims @type{$\tau$} about the value of an untyped expression, the gradual type system inserts a runtime check @ctc{$\tau$} capable of deciding whether an arbitrary value belongs to the denotation of the syntactic type @type{$\tau$}.
If, at runtime, the expression does not produce a value satisfying @ctc{$\tau$}, the program halts with a so-called @emph{type boundary error}.

@; @note{In fact, Ren and Foster recently applied just-in-time type checking to six Ruby programs and found @emph{zero} latent type errors.}
   @remark[]{
     Type boundary errors arise when type annotations impose new constraints that untyped code does not satisfy.
     Such errors may indicate latent bugs in the untyped code @emph{or} mistakes in the new type annotations.
     Put differently, the slogan @exact{``well-typed programs can't be blamed''}@~cite[wf-esop-2009] does not match the philosophy of gradual typing.
   }@;
@;
For example, the Typed Racket function in @Figure-ref{fig:complex-multiply} implements multiplication for polar-form complex numbers.@note{Racket and Typed Racket have native support for complex numbers.} @; This is just an example@~cite[r-ip-1983].}
Suppose a gradually typed program uses this function.
Gradual type soundness demands that at runtime, every value that flows from untyped code to @racket[reynolds-*] passes the predicate @ctc{JR}.
@; If there are @math{n} calls to the function from untyped contexts, all @math{2n} values factor through the predicate.

These checks are indispensible because Typed Racket's static types operate at a higher level of abstraction than Racket's dynamic typing.
In particular, the tag checks performed by @racket[first] and @racket[*] do not enforce the @type{JR} type.
If we were to omit the @ctc{JR} check, the call
    @racket[(reynolds-* '(-1 0) '(-3 0))]
 would produce a well-typed complex number from two ill-typed inputs.
Racket's runtime would not detect this erroneous behavior, therefore the program would silently go wrong.
Such are the dangers of committing ``moral turpitude''@~cite[r-ip-1983].

Furthermore, even if a tag check uncovers a logical type error, debugging such errors in a higher-order functional language is often difficult.
Well-trained functional programmers follow John Hughes' advice and compose many small, re-usable functions to build a program@~cite[h-cj-1989].
Unfortunately, this means the root cause of a runtime exception is usually far removed from the point of logical failure.
@; Laziness does not pay in the context of type checking.

In contrast, sound gradual typing guarantees that typed code never executes a single instruction using ill-typed values.
Programmers can trust that every checked type annotation is a true statement.
Furthermore, the interposed predicates detect type boundary errors as soon as possible.
If such an error occurs, the runtime enforcement system points the programmer to the relevant type annotation and supplies the incompatible value as a witness to the logical mistake.
These guarantees and improvements in developer productivity are valuable, but come at the price of runtime checks.
Therefore, we need a performance evaluation method to quantify their cost.

    @figure["fig:complex-multiply" "Multiplication for polar form complex numbers"
      @(begin
      #reader scribble/comment-reader
      @codeblock|{
        #lang typed/racket

        (provide reynolds-*)

        (define-type JR (List Nonnegative-Real Real))
        ;; JR = (Distance from origin, Radians)

        (: reynolds-* (JR JR -> JR))
        (define (reynolds-* jr1 jr2)
          (list (* (first  jr1) (first  jr2))
                (+ (second jr1) (second jr2))))
      }|)
    ]

