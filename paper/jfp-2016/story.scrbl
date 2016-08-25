#lang scribble/base

@require["common.rkt" "benchmark.rkt" "util.rkt" racket/file]

@profile-point{sec:story}
@title[#:tag "sec:story"]{Gradual Typing in Typed Racket}

Typed Racket@~cite[TypedRacket] is the oldest and most developed implementation of sound gradual typing.
It supports clients in both academia and industry.
Typed Racket attracts these clients because it supports the idioms of Racket.
In fact, a major design goal of Typed Racket is that equipping a Racket program
 with types is only a matter of annotating function parameters, class fields, and recursive definitions.
The underlying Racket code can remain unchanged, including code using
 variable-arity polymorphism@~cite[stf-esop-2009],
 first-class classes@~cite[tsdthf-oopsla-2012],
 delimited continuations@~cite[tsth-esop-2013],
 or contracts@~cite[l-mthesis-2016].
Finally, a typed module may incorporate definitions from a Racket module with a type-annotated import statement;
 a Racket module may use definitions from a Typed Racket module without knowledge that the provider is typed.

Due to the close integration of Racket and Typed Racket, programmers frequently use both languages within a single program.
Furthermore, programmers often migrate Racket modules to Typed Racket as the software evolves.
In general one cannot predict why or how such conversions happen, but some
 common motivations are:
@itemlist[
  @item{
    The typechecker provides @emph{assurance} against common bugs, for example,
     forgetting to check for end-of-file symbols when reading from a port.
  }
  @item{
    Racket libraries maintain an interface of @emph{compatible} type annotations,
     saving typed clients the need to supply their own.
  }
  @item{
    Type signatures serve as machine-enforced @emph{documentation}.
  }
  @;@item{
  @;  Programmers convert modules that are @emph{simple} to type.
  @;  These modules could be the smallest, or have the fewest dependencies.
  @;}
  @item{
    Typed modules gain @emph{performance} improvements from the Typed Racket optimizer@~cite[stf-optimization-coaching]
     and, unlike untyped modules, suffer no dynamic cost if tightly-coupled to other typed modules.
  }
]
Let us illustrate one possible evolution.
Imagine a large repository in which an error is traced to an untyped module.
In all likelihood, the programmer tasked with fixing the bug must recover the type specifications that the original developer had in mind---but did not write down.
Doing so requires studying the code and analyzing its unit tests, a significant burden.
Typed Racket can preserve these specifications for future maintainers,
 provided the programmer makes the investment of formalizing the recovered types.
@; a little scatterbrained


@; -----------------------------------------------------------------------------
@section{The Costs of Incremental Typing}

Adding types to an untyped module is a tradeoff.
Performing the type conversion may yield long-term benefits,
 but incurs immediate engineering costs.

The first cost is the burden of writing type annotations.
In particular, Typed Racket is a @emph{macro-level}@note{As opposed to @emph{micro-level}, see @secref{sec:flavors}.} gradual type system.
Every expression within a Typed Racket module must pass the type checker.
Consequently, every function needs a complete type signature and every class field needs an explicit type.
Maintaining these annotations is another cost.

The second cost is the risk of introducing bugs during the conversion.
Typed Racket mitigates this risk by accomodating many Racket idioms,
 but occasionally programmers must refactor code to satisfy the type checker.
Refactoring is always a chance to introduce bugs.

The third cost is performance overhead.
Converting a single module to Typed Racket introduces so-called @emph{type boundaries} to neighboring untyped modules.
Values that cross a type boundary at runtime must pass a dynamic type check;
 these checks may introduce significant runtime overhead.
For example, Typed Racket developers have experienced pathologies including
 a 50% overhead in a commercial web server
 and 25x-50x slowdowns when using the math library.@note{See the appendix for a list of user reports.}
One potential solution is to convert additional modules to Typed Racket.
Indeed, one user reported a speedup from @|PFDS-BEFORE| to @|PFDS-AFTER| after converting a script to Typed Racket.
But annotating one additional module is not guaranteed to improve performance and may introduce new type boundaries.

Of these three costs, the performance overhead is the most troublesome.
Part of the issue is that the cost is implicit; unlike the tangible cost of
 writing type annotations or the perennial risk of introducing bugs, the dynamic
 cost of enforcing type soundness is not apparent until runtime.
Additionally, experience with other languages does not give an intuition for the performance cost of gradual typing.
Programmers familiar with statically typed languages do not expect any overhead.
Programmers familiar with optionally-typed dynamic languages, such as TypeScript, also expect that types impose no runtime cost.
Typed Racket insists on gradual type soundness, therefore types need runtime enforcement.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:overhead"]{A Case for Sound Gradual Typing}

Recall that types are checkable statements about program expressions.
Soundness means that every checked statement always holds as the program is executed.
Statically typed languages provide this guarantee by checking every expression.
In a gradually typed languages, the type system cannot check every expression because some are intentionally unannotated.
Therefore sound gradual type systems chaperone the interaction of typed and untyped program components at runtime.
Where typed code claims an untyped expression has type @type{$\tau$}, the gradual type system interposes a runtime check @ctc{$\tau$} capable of deciding whether an arbitrary value belongs to the denotation of the syntactic type @type{$\tau$}.
If this predicate @ctc{$\tau$} fails, the program halts with a @emph{type boundary error}.

   @remark[]{
     Type boundary errors arise when type annotations impose new constraints that untyped code does not satisfy.
     Such errors may indicate latent bugs in the untyped code @emph{or} mistakes in the new type annotations.
     Put differently, the slogan @exact{``well-typed programs can't be blamed''}@~cite[wf-esop-2009] does not match the philosophy of gradual typing.
   }

   @; type boundary?

For example, the Typed Racket function in @Figure-ref{fig:complex-multiply} implements multiplication for polar-form complex numbers.@note{Racket and Typed Racket have native support for complex numbers.} @;This example is adapted from Reynolds' paper@~cite[r-ip-1983].
Suppose a gradually typed program uses this function.
Gradual type soundness demands that at runtime, every value that flows from untyped code to @racket[reynolds-*] passes the predicate @ctc{JR}.

These checks are indispensible because Typed Racket's static types operate at a higher level of abstraction than Racket's dynamic typing.
In particular, the tag checks performed by @racket[first] and @racket[*] do not enforce the @type{JR} type.
Omitting the @ctc{JR} check permits the call
    @racket[(reynolds-* '(-1 -1) '(-3 0))],
 which produces a well-typed complex number from two ill-typed inputs.
Racket's runtime cannot detect this erroneous behavior.
The program and the program's users receive no clue how the program went wrong.
Such are the dangers of committing ``moral turpitude''@~cite[r-ip-1983].

Furthermore, even if a tag check uncovers an error that sound gradual typing would have caught, debugging such errors in a higher-order functional language is often difficult.
Well-trained functional programmers follow John Hughes' advice and compose their programs from small, highly re-usable functions@~cite[h-cj-1989].
Unfortunately, this means the root cause of a runtime exception is usually far removed from the point of logical failure.
Laziness does not pay in the context of type checking.

In contrast, sound gradual typing guarantees that typed code never executes a single instruction using ill-typed values.
Programmers can trust that every type annotation is a true statement.
Furthermore, the interposed predicates detect type boundary errors as soon as possible.
If a type boundary error occurs, the runtime enforcement system points the programmer to the relevant type and incompatible value, thus narrowing the search space for the logical mistake.
Runtime checks are the price for these guarantees and improvements in productivity.
We therefore need a performance evaluation method to quantify their cost.

    @figure["fig:complex-multiply" "Multiplication for polar form complex numbers"
      @(begin
      #reader scribble/comment-reader
      @codeblock|{
        #lang typed/racket

        (define-type JR (List Nonnegative-Real Real))
        ;; JR = (Distance from origin, Radians)

        (: reynolds-* (JR JR -> JR))
        (define (reynolds-* jr1 jr2)
          (list (* (first jr1) (first jr2))
                (+ (second jr1) (second jr2))))
      }|)
    ]

