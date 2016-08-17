#lang scribble/base

@require["common.rkt" "benchmark.rkt" "util.rkt" racket/file]

@profile-point{sec:story}
@title[#:tag "sec:story"]{Gradual Typing in Typed Racket}

Typed Racket@~cite[TypedRacket] is the oldest and most developed implementation of sound gradual typing.
It supports clients in both academia and industry.@note{@url{http://con.racket-lang.org/2015/burns.pdf}}
   @; as well as a few prolific hobbyists.@note{MB}
These users frequently report steep performance penalties when they mix Racket and Typed Racket modules in a single application.
Nonetheless, programmers continue to use Typed Racket because it accomodates the idioms of Racket and helps catch "subtle reasoning errors".@note{Personal communication with one Typed Racket user.}
For example, the widely-used math, statistics, and plotting libraries are all typed.

Typed Racket programs, as opposed to scripts, frequently incorporate untyped components and use untyped libraries.
Conversely, untyped programs often depend on typed libraries.
In other words, gradual typing is widespread.

The prevalence of gradual typing is largely due to the low syntactic burden
 of mixing typed and untyped code.
Racket programmers may import most typed values as if they were untyped.
The only restriction is that typed compiler extensions cannot run in untyped contexts.
Typed Racket programmers can import untyped values by giving each import an
 explicit type annotation.

Converting a module between the Racket and Typed Racket languages is also straightforward.
The first line in every module declares its language, e.g. 
 @hash-lang[] @racket[racket] or @hash-lang[] @racket[typed/racket].
Typed Racket modules require annotations on function parameters, class fields,
 mutable variables, and untyped imports; otherwise, the syntax of Racket and Typed Racket is identical.
In particular, removing all type annotations from a Typed Racket module yields
 a syntactically valid Racket module.

Given the close integration of Racket and Typed Racket, programmers tend to
 incrementally type untyped modules.
Below are a few common use cases, but in general we cannot predict why or how programmers choose to add types.
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
Imagine a large codebase in which an error is traced to an untyped module
 whose author has long since left the project.
Unless the internals of this module have documentation, the new programmer
 tasked with fixing the bug must recover the implicit type specifications
 throughout the module by reading the code and analyzing its unit tests.
This type recovery process might take hours or weeks.

After fixing the bug, the programmer can formally state these recovered specifications by converting the module to Typed Racket.
If the programmer takes this step, the module is now properly documented with type annotations for future maintainers.
@; Types may spread in this manner throughout the codebase
@; (contracts are infectious)

@; -----------------------------------------------------------------------------
@section{The Costs of Type Conversion}

Even in the scenario outlined above, where the programmer likely spent hours
 recovering type information, converting an untyped module to Typed Racket is
 orthogonal to programmers' main goal of delivering a working software product.
If the programmer does attempt to formalize types, there are other tradeoffs to bear in mind.

For one, Typed Racket is a @emph{macro-level} gradual type system.
Every statement within a @hash-lang[] @racket[typed/racket] module must pass typechecking.
In practice, this means that every function needs a complete type signature
 and every @tt{while} or @tt{for} loop needs an explicit return type.
The type annotations a programmer has in mind are almost certainly fewer than what the type checker requires.@note{@emph{Micro-level} gradual typing is an alternative, see @secref{sec:flavors} for a comparison.}

Second, any refactoring is an unnecessary chance to introduce bugs.
If converting to Typed Racket is @emph{only} a matter of adding type annotations
 then the risk is minimized, but type checkers cannot always follow programmers' reasoning.

Third, and most importantly, converting a single module to Typed Racket introduces type boundaries to neighboring untyped modules.
Values that cross type boundaries at runtime must pass a dynamic type check;
 these checks may entail significant runtime overhead.
For example, Typed Racket developers have experienced pathologies including
 a 50% overhead in a commercial web server
 and 25x-50x slowdowns when using the math library.
One potential solution is to convert additional modules to Typed Racket.
Indeed, one user reported a speedup from @|PFDS-BEFORE| to @|PFDS-AFTER| after converting a script to Typed Racket.
But annotating one additional module is not guaranteed to improve performance and may introduce new type boundaries.


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

At first glance, the proxy may seem overly conservative.
After all, untyped Racket is a memory safe programming language.
If untyped code replaces the contents of a voting machine with a @type{String}, then the program is sure to raise an exception when it treats the string as a number.

The true danger is that untyped code might put an ill-typed number such as a @type{Real} or @type{Negative-Integer} in the machine.
Suppose the program manages several voting machines and eventually sums their contents.
If one voting machine contains a real number, the program will silently compute a nonsensical sum.
The whole purpose of a type system is to prevent such failures.
A proxy fulfills this purpose by detecting and immediately reporting
 any mismatch between the type system and runtime values.

   @remark[]{
     A runtime type error only demonstrates a value that the type system did not expect.
     Neither the typed code nor the untyped code is necessarily at fault.
     @; The type may be wrong; this is especially likely when one adds type annotations to untyped code.
   }

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

For a concrete example, the Typed Racket function in @figure-ref{fig:complex-multiply}
 implements multiplication for complex numbers represented in polar form.
The type system guards calls to @racket[reynolds-*] from untyped code with the
 higher-order contract @ctc{JR JR -> JR}.
In other words, untyped clients must supply two pairs of real numbers where the first component of each pair is non-negative.

Each call to the protected @racket[reynolds-*] requires a total of six assertions to check and traverse both pairs.
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

