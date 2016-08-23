#lang scribble/base

@require["common.rkt" "benchmark.rkt" "util.rkt" racket/file]

@profile-point{sec:story}
@title[#:tag "sec:story"]{Gradual Typing in Typed Racket}

Typed Racket@~cite[TypedRacket] is the oldest and most developed implementation of sound gradual typing.
@; TODO "it ... it" bo-ring
It supports clients in both academia and industry.@note{See @url{http://www.google.com} for a partial list.}
Typed Racket attracts these clients because it supports the idioms of Racket.
In fact, a major design goal of Typed Racket is that converting a Racket program
 to Typed Racket is only a matter of adding type annotations.
Existing code can remain unchanged, including code using
 variable-arity procedures@~cite[stf-esop-2009],
 first-class classes@~cite[tsdthf-oopsla-2012],
 delimited continuations@~cite[tsth-esop-2013].
 or contracts@~cite[l-mthesis-2016].
Conversely, removing all types from a Typed Racket program yields a syntactically valid Racket program.

Due to the close integration of Racket and Typed Racket, programmers frequently use both languages within a single program.
Furthermore, programmers often migrate Racket modules to Typed Racket as the software evolves.
In general one cannot predict why or how such conversions happen, but some
 common motivations for Typed Racket are:
@itemlist[
  @item{ @bold{Assurance:}
    The typechecker guards against common bugs, for example,
     forgetting to check for end-of-file symbols when reading from a port.
  }
  @item{ @bold{Compatibility:}
    Maintainers of untyped libraries provide type annotations for library exports,
     saving Typed Racket clients the need to supply their own.
  }
  @item{ @bold{Documentation:}
    Type signatures serve as machine-enforced documentation.
  }
  @;@item{ @bold{Ease:}
  @;  Programmers convert those modules that are the least effort to type.
  @;  These modules could be the smallest, or have the fewest dependencies.
  @;}
  @item{ @bold{Performance:}
    Typed modules benefit from the Typed Racket optimizer@~cite[stf-optimization-coaching]
     and, unlike untyped modules, suffer no dynamic cost if tightly-coupled to other typed modules.
  }
]
Let us illustrate one possible evolution.
Imagine a large repository in which an error is traced to an untyped module
 whose author has long since left the project.
In all likelihood, the new programmer
 tasked with fixing the bug must recover the implicit type specifications
 throughout the module by reading the code and analyzing its unit tests.
After recovering these specifications, the programmer may preserve the types for future maintainers
 by converting the module to Typed Racket.

This scenario is interesting for two reasons.
First, the newly-typed module is essentially a random component of the program.
Second, a significant part of the conversion process (recovering type information)
 was part of the programmer's necessary workflow.
Finishing the conversion, however, is still an investment.


@; -----------------------------------------------------------------------------
@section{The Costs of Type Conversion}

Adding types to an untyped module is a tradeoff.
Performing the type conversion may yield long-term benefits,
 but incurs immediate engineering costs.

The first cost is the burden of writing type annotations.
In particular, Typed Racket is a @emph{macro-level}@note{As opposed to @emph{micro-level}, see @secref{sec:flavors}.} gradual type system.
Every statement within a Typed Racket module must pass the type checker.
Consequently, every function needs a complete type signature and every @tt{while} or @tt{for} loop needs an explicit return type.
Maintaining these annotations is another near-term cost.

The second cost is the risk of introducing bugs during the conversion.
Typed Racket mitigates this risk by accomodating Racket idioms. @;, but occasionally programmers must edit code to satisfy the type checker.
Even so, any refactoring is a chance to introduce bugs.

The third cost is performance overhead.
Converting a single module to Typed Racket introduces type boundaries to neighboring untyped modules.
Values that cross type boundaries at runtime must pass a dynamic type check;
 these checks may introduce significant runtime overhead.
For example, Typed Racket developers have experienced pathologies including
 a 50% overhead in a commercial web server
 and 25x-50x slowdowns when using the math library.
One potential solution is to convert additional modules to Typed Racket.
Indeed, one user reported a speedup from @|PFDS-BEFORE| to @|PFDS-AFTER| after converting a script to Typed Racket.
But annotating one additional module is not guaranteed to improve performance and may introduce new type boundaries.

Of these three costs, the performance overhead is the biggest pain point for users.
Part of the trouble is that the cost is implicit; unlike the tangible cost of
 writing type annotations or the perennial risk of introducing bugs, the dynamic
 cost of enforcing type soundness is not apparent until runtime.
Additionally, experience with other languages does not give an intuition for gradual typing.
Programmers familiar with statically typed languages do not expect any overhead.
Programmers familiar with optionally-typed dynamic languages, like TypeScript,
 also expect that types will be erased.
Typed Racket insists on type soundness, therefore types need runtime enforcement.
@; TODO weaaak ending


@; -----------------------------------------------------------------------------
@section[#:tag "sec:overhead"]{A Case for Sound Gradual Typing}

Typed Racket is a sound type system.
Every statement it makes about a variable during compilation holds true for
 all values that variable represents at runtime.
@; TODO abstract explanation? (yes)
This is possible because every Typed Racket type @type{T} has a computational
 interpretation @ctc{T} capable of deciding whether arbitrary (untyped) values belong to
 the syntactic type @type{T}.

As a simple example, suppose we represent a voting machine as a mutable cell
 containing a natural number.
The Typed Racket type for such a value is @racket[(Boxof Natural)].
Typed code that interacts with a voting machine must evidently adhere to this type;
 if not, type checking halts with a compile-time type error.
Untyped code that interacts with a voting machine is not subject to type checking.
Nevertheless, the type system must prevent untyped code from replacing the contents
 of a voting machine with values that have e.g. the static types @type{String}
 or @type{Negative-Real}.
It does so by compiling the type to a contract @ctc{Boxof Natural} and applying this contract to voting machines that cross a type boundary.
The contract proxies a voting machine and dynamically enforces type-correct use by checking every write to the machine.

The proxy may seem overly conservative.
After all, untyped Racket is a dynamically typed programming language.
If untyped code replaces the contents of a voting machine with a @type{String}, then the program is sure to raise an exception later, when it treats the string as a number.

On the contrary, there are two deep motivations for enforcing type soundness.
The first is that dynamic type errors are hard to diagnose in higher-order functional languages.
Programmers who follow John Hughes' advice and compose their programs from small, re-usable functions@~cite[h-cj-1989] will find that the cause of a dynamic type error is often far removed from the point of failure.
Enforcing type soundness catches such errors immediately, as soon as an untyped value contradicts a static type annotation.
@; laziness does not pay for debugging -- the static guys know this already

The second reason is that dynamic checks are not faithful to all static types.
Tag-checking enforces minimal constraints;
 na@exact{\"i}vely substituting tags for types opens the door to silent semantic failures.
Suppose a program manages several voting machines and eventually sums their contents.
If untyped code puts a real number in one voting machine, the program will still compute a sum without error.
The result, however, will be nonsense.
This is precisely the sort of nonsense a type system is supposed to prevent.

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

   @remark[]{
     Runtime @emph{type boundary errors} arise when type annotations impose
      new constraints that untyped code does not live up to.
     Such errors may indicate latent bugs in the untyped code, but the fault may lie in the @emph{new} type annotations.
     Put differently, the slogan @exact{``well-typed programs can't be blamed''} does not match the pragmatics of gradual typing.
   }

As a second example, the Typed Racket function in @figure-ref{fig:complex-multiply}
 implements multiplication for complex numbers represented in polar form.
The type system guards @racket[reynolds-*] from untyped code with the
 higher-order contract @ctc{JR JR -> JR}.
In other words, untyped clients must supply two pairs of real numbers where the first component of each pair is non-negative.

Each call to the protected @racket[reynolds-*] requires six assertions to check and traverse both pairs.
Individual assertions are relatively inexpensive, but folding
 @racket[reynolds-*] over a sequence of @math{n} complex numbers requires
 @math{3n + 1} assertions.
The cost of such checks quickly accumulates.
But these checks are again indispensable.
Without them, ill-typed calls such as:
    @racketblock[(reynolds-* '(-1 -1) '(-3 0))]
 fail silently.
If we are lucky, the silent failure triggers an error later in the program,
 but because the polar product of @racket['(-1 -1)] and @racket['(-3 0)] is
 the well-typed complex number @racket['(3 -1)], it is more likely that the
 program computes an incorrect result.
The programmer and the program's users receive no clue that an erroneous computation occurred.
Such are the dangers of committing ``moral turpitude''@~cite[r-ip-1983].

    @; add example to show "surprising costs"?
    @; - Derivatives of complex numbers
    @; - Taylor series expansion
    @; The program in @Figure-ref{TODO} underscores this point.
    @; It is a script for (... tree / list ... complex ...)
    @; When the script is typed, it runs in XXXms. Untyped, the same code takes
    @;  YYYms to run.
    @; TODO connect example to soundness?

