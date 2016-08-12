#lang scribble/base

@; TODO
@; - fix suffixtree numbers
@; - orphans/widows

@require["common.rkt" "benchmark.rkt" "util.rkt"]
@(require racket/file benchmark-util/data-lattice)

@profile-point{sec:story}
@title[#:tag "sec:story"]{Gradual Typing in Typed Racket}

Typed Racket@~cite[TypedRacket] is the oldest implementation of sound gradual typing and
 supports clients in both academia and industry@~cite[b-rc-2015].
   @; as well as a few prolific hobbyists.@note{MB}
Users frequently encounter performance overhead when mixing Racket and Typed
 Racket modules, so it is a natural context for our performance evaluation.

@; TR is for real, TR is gradual
Typed Racket implements so-called @emph{macro-level} gradual typing; that is,
 every module in a Racket program is explicitly typed or
 untyped.@note{Typed Racket
    does allow typed expressions and definitions within untyped modules, but our
    benchmarks do not use these so-called @emph{typed regions}.}
Programmers opt-in to Typed Racket by writing @hash-lang[] @racket[typed/racket]
 at the top of a module.
In contrast, @hash-lang[] @racket[racket] modules are untyped.
The syntax of Typed Racket is a superset of Racket, so that converting a
 Racket module to Typed Racket is primarily a matter of writing type annotations.
Intuitively, modifying working untyped code just to satisfy the type checker
 is an unnecessary opportunity to introduce bugs.

    @;bg; insert a figure here?
    @;, as demonstrated in @figure-ref{fig:racket-v-typed}.
    @; @figure*["fig:racket-v-typed" "Merging lists in Racket (left) and Typed Racket (right)"
    @;   @exact|{\input{fig-racket-v-typed}}|
    @; ]

When a typed module imports definitions from an untyped module, programmers
 must supply type annotations.
The type system compiles these annotations to
 contracts@~cite[ff-icfp-2002] that ensure the untyped code matches its type specification.
Untyped modules may use definitions from typed modules without any annotation.
The type system generates contracts to protect the typed values from untyped contexts.
Put another way, every @emph{logical} type @exact|{$\RktMeta{T}$}| in Typed
 Racket has a @emph{computational} projection @exact|{$\ctc{\RktMeta{T}}$}|
 capable of dynamically checking whether an arbitrary value has type @exact|{$\RktMeta{T}$}|.

Racket programmers frequently mix typed and untyped code.
For example, the standard matrix, statistics, and plotting libraries are implemented
 in Typed Racket and used by many untyped programs.
Conversely, Racket's core libraries are untyped and used by all typed programs.
Within a single project, we find that Typed Racket users have diverse motivations
 for choosing which modules to type and which to leave untyped.
Some of the most common use-cases are:
@itemlist[
  @item{ @bold{Assurance:}
    The typed modules implement core functionality and the typechecker
     guards against common bugs; for example, forgetting to check
     for end-of-file symbols when reading from a port.
  }
  @item{ @bold{Documentation:}
    The typed modules have stable interfaces.
    Type signatures serve as machine-checked and enforced APIs.
  }
  @item{ @bold{Ease:}
    The typed modules are the least effort to type.
    These could be the smallest modules or the ones with the fewest dependencies.
  }
  @item{ @bold{Necessity:}
    The typed modules are tightly-coupled to other typed modules or to typed library code.
    Tight coupling between an untyped module and a typed modules leads to frequent dynamic type checks.
    Typing both modules statically discharges the runtime checks.
  }
]
Over time, programmers may convert additional modules in a program to Typed Racket.
Imagine for example a large codebase in which a bug is traced to an untyped module deep within the project.
The module is sparsely documented and its original author has long since left the project.
The new programmer tasked with fixing this bug is forced to read the code
 and its unit tests to uncover the @emph{implicit} specifications of components in the module.
Armed with these specifications, the programmer can finally address the bug.

Typed Racket makes it easy to convert these inferred specifications into type signatures.
The programmer only needs to write these implicit signatures, annotate imports
 from untyped modules, and change the @hash-lang[] line from @racket[racket]
 to @racket[typed/racket].
After converting, the module is now hardened against bugs due to type errors
 and documented with type annotations for future maintainers.

Introducing types in one module while leaving others untyped, however,
 introduces type boundaries that may cause significant performance overhead.
For example, Typed Racket developers have experienced pathologies including
 a 50% overhead in a commercial web server
 and 25x-50x slowdowns when using the standard math library.
One potential solution is to convert additional modules to Typed Racket.
Indeed,
 one user reported a speedup from @|PFDS-BEFORE| to @|PFDS-AFTER| after converting
 a script from Racket to Typed Racket.
But annotating a module is not guaranteed to improve performance and may introduce
 new, high-overhead type boundaries.

As language designers, we must also remember that adding types is always
 a software engineering burden.
Even in the @emph{campground} @note{Golden rule of camping: always leave the campground cleaner than you found it.}
 scenario outlined above, converting untyped code to
 Typed Racket is orthogonal to developers' primary goal of delivering a working
 software product.
@; Quote Matthew?
We must therefore improve the @emph{implementation} of gradual typing rather
 than leave performance issues to language users.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:overhead"]{The Source of Performance Overhead}

Boundaries between typed and untyped modules are the source of the performance costs
 in Typed Racket.
As a concrete example, @figure-ref{fig:complex-multiply} implements
 multiplication for complex numbers represented in polar form.
When this typed function is imported by an untyped module, we say it crosses a
 @emph{type boundary}.
During the import, Typed Racket compiles the function's type signature to a
 higher-order contract @exact|{$\ctc{\RktMeta{(C C -> C)}}$}|.
The contract asserts that each call to @racket[complex-*] in untyped code
 supplies two arguments passing the @exact|{$\ctc{\RktMeta{C}}$}| contract;
 that is, two pairs of real numbers where the first component of each pair is
 non-negative.
Consequently, the contract detects invalid arguments such as the string @racket{NaN}
 and immediately blames@~cite[ff-icfp-2002] the caller for making a type error.

    @figure["fig:complex-multiply" "Multiplication for polar form complex numbers"
      @(begin
      #reader scribble/comment-reader
      @codeblock|{
        #lang typed/racket

        (define-type C (Pairof Nonnegative-Real Real))
        ;; C = (Distance from origin, Radians)

        (: complex-* (C C -> C))
        (define (complex-* c1 c2)
          (cons (* (car c1) (car c2))
                (+ (cdr c1) (cdr c2))))
      }|)
    ]

Correct blame is essential to Typed Racket's soundness guarantee, which states
 that typed code is never the cause of runtime type errors.
Rather, all runtime type errors are dynamically caught and attributed to a type boundary.
Of course, tracking blame and dynamically enforcing types can be computationally expensive.
Each call to @racket[complex-*] requires in total six assertions to check and traverse both pairs.
Individual assertions are relatively inexpensive, but folding
 @racket[complex-*] over a list of @math{n} complex numbers requires
 @math{3n + 1} assertions.
In general, costs can quickly accumulate as the size of data
 and number of calls to @racket[complex-*] increases.

    @; TODO example here, to show "surprising costs"?
    @; - Derivatives of complex numbers
    @; - Taylor series expansion
    @; The program in @Figure-ref{TODO} underscores this point.
    @; It is a script for (... tree / list ... complex ...)
    @; When the script is typed, it runs in XXXms. Untyped, the same code takes
    @;  YYYms to run.
    @; TODO connect example to soundness?

Despite the potential overhead, it is crucial that calls to @racket[complex-*]
 are guarded against type errors even though untyped Racket is a memory-safe
 programming language and will detect e.g. inputs like the string @racket{NaN}
 when they reach the @racket[+] function.
The danger is with ill-typed calls such as:
    @racketblock[(complex-* '(-1 . -1) '(-3 . 0))]
 which fail @emph{silently}.
If we are lucky, the silent failure will trigger an error later in the program,
 but since the polar product of @racket['(-1 . -1)] and @racket['(-3 . 0)] is
 the well-typed complex number @racket['(3 . -1)] it is more likely that the
 program will compute an incorrect result and leave the programmer no clue as to
 where the error occurred.
Such are the dangers of committing ``moral turpitude''@~cite[r-ip-1983].

