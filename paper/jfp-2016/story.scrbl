#lang scribble/base

@; TODO
@; - fix suffixtree numbers
@; - orphans/widows

@require["common.rkt" "typed-racket.rkt"]
@(require racket/file benchmark-util/data-lattice)

@profile-point{sec:story}
@title[#:tag "sec:story"]{Gradual Typing in Typed Racket}

@; TR is for real, TR is gradual
Our research is motivated by practical experience with Typed Racket@~cite[TypedRacket],
 the gradually-typed sister language of Racket@~cite[plt-tr1].
Typed Racket implements @emph{macro-level} gradual typing; that is,
 every module in a Racket program is explicitly typed or untyped.@note{Typed Racket
  does allow typed expressions and definitions within untyped modules, but our
  benchmarks do not use these so-called @emph{typed regions}.}
Programmers invoke the type checker by writing @hash-lang[] @racket[typed/racket]
 at the top of a module.
In contrast, @hash-lang[] @racket[racket] modules are untyped.
The syntax of Typed Racket is a superset of Racket, the difference being the
 statically-checked type annotations that Typed Racket programmers use to
 document their intentions and guide the type checker.
@; TODO ew
Converting a program between Racket and Typed Racket is by design only a matter
 of adding or removing type annotations.

 @;bg; insert a figure here?
 @;, as demonstrated in @figure-ref{fig:racket-v-typed}.
 @; @figure*["fig:racket-v-typed" "Merging lists in Racket (left) and Typed Racket (right)"
 @;   @exact|{\input{fig-racket-v-typed}}|
 @; ]

When a typed module uses definitions from an untyped module, programmers
 must supply type annotations in the import statement.
These annotations are compiled to
 contracts@~cite[ff-icfp-2002] to ensure that the dynamic behavior of the untyped
 code matches its type specification.
Untyped modules may use definitions from typed modules without any annotation.
The imported typed code is protected with automatically generated
 contracts to enforce type-correct use in untyped contexts.
Put another way, every @emph{logical} type @exact|{$\RktMeta{T}$}| in Typed
 Racket has a @emph{computational} projection @exact|{$\ctc{\RktMeta{T}}$}|
 capable of checking whether an arbitrary value has type @exact|{$\RktMeta{T}$}|.

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
    The typed modules are stable modules whose interfaces are unlikely to
     change.
    Type signatures serve as machine-checked and enforced APIs.
  }
  @item{ @bold{Ease:}
    The typed modules are the least effort to type.
    These could be the smallest modules or the ones with the fewest dependencies.
  }
  @item{ @bold{Necessity:}
    The typed modules are tightly-coupled to other typed modules or to typed
     library code.
    Such coupling between a typed module and an untyped module may entail
     a large run-time cost due to checking contracts on the type boundary.
    Typing both modules eliminates the boundary.
  }
]
Over time, programmers may convert additional modules in a program to Typed Racket.
Consider a large untyped codebase in which a bug is traced to an untyped
 module that was written years ago by a developer who is no longer with the project.
The programmer tasked with fixing the bug must first understand how the module
 is intended to work by reading documentation, unit tests, and the code; only
 then can they fix the issue.
A side effect of this investigation is that the programmer uncovers the @emph{implicit}
 type signatures of functions and datatypes in the module.
Typed Racket makes it easy to record these types.
The programmer only needs to write these implicit signatures, annotate imports
 from untyped modules, and change the @hash-lang[] line from @racket[racket]
 to @racket[typed/racket].
After converting, the module is now hardened against bugs due to type errors
 and documented with type annotations for future maintainers.

Introducing types in one module while leaving others untyped, however,
 introduces type boundaries that may lead to significant performance overhead.
For example, Typed Racket developers have experienced pathologies including
 a 50% overhead in a commercial web server
 and 25x-50x slowdowns when using the standard math library.
One potential solution is to convert additional modules to Typed Racket.
Indeed,
 one user reported a speedup from @|PFDS-BEFORE| to @|PFDS-AFTER| after converting
 a script from Racket to Typed Racket.
But annotating a module is not guaranteed to improve performance and may introduce
 new type boundaries with even larger overhead.

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
During the import, the function's type signature is compiled into a
 higher-order contract @exact|{$\ctc{\RktMeta{(C C -> C)}}$}|
 to dynamically enforce correct usage.
The contract asserts that each call to @racket[complex-*] in untyped code
 is made with two arguments accepted by the @exact|{$\ctc{\RktMeta{C}}$}| contract;
 that is, two pairs of real numbers where the first component of each pair is
 non-negative.
Invalid arguments such as the string @racket{NaN}
 are detected immediately, before @racket[complex-*] is invoked, and
 the untyped code is blamed@~cite[ff-icfp-2002] for supplying arguments of the wrong type.

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
 that typed code is never the cause of run-time type errors.
Rather, all type errors are either detected statically or dynamically caught
 and attributed to a type boundary.
Tracking blame and dynamically enforcing types can, however, be computationally expensive.
Each call to @racket[complex-*] requires six assertions to check and traverse both pairs.
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
 programming language and will catch e.g. inputs like the string @racket{NaN}
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

