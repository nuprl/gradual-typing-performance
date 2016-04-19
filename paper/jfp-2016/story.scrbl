#lang scribble/base

@; TODO
@; - fix suffixtree numbers
@; - orphans/widows

@require["common.rkt" "typed-racket.rkt"]
@(require racket/file benchmark-util/data-lattice)

@title[#:tag "sec:story"]{Gradual Typing in Typed Racket}

@; TR is for real, TR is gradual
Our research is motivated by practical experience with Typed Racket@~cite[TypedRacket],
 the gradually-typed sister language of Racket@~cite[plt-tr1].
Typed Racket implements macro-level gradual typing.
Programmers choose to invoke the type checker by writing @exact{\RktMeta{\#lang typed/racket}}
 at the beginning of a module.
In contrast, modules declared using @exact{\RktMeta{\#lang racket}} are untyped.
When a typed module relies on bindings from an untyped module, programmers
 must supply type annotations in the import statement.
These annotations are compiled to
 contracts@~cite[ff-icfp-2002] to ensure that the dynamic behavior of the untyped
 code matches its type specification.
Untyped modules may freely use bindings from typed modules.
The imported typed code is protected with automatically generated
 contracts to ensure type-correct use in untyped contexts.

Racket programmers frequently mix typed and untyped code.
For example, the standard matrix, statistics, and plotting libraries are implemented
 in Typed Racket and used by many untyped programs.
Conversely, Racket's core libraries are untyped and used by nearly all typed programs.
Within a single project, we find that Typed Racket users have diverse motivations
 for choosing which modules to type and which to leave untyped.
Some of the most common use-cases are:
@itemlist[
  @item{ @bold{Assurance:}
    The typed modules implement core functionality that the programmer
     wanted to guard against common bugs; for example, forgetting to check
     for end-of-file symbols when reading from a port.
  }
  @item{ @bold{Documentation:}
    The typed modules are stable modules whose interfaces are unlikely to
     change.
    Type signatures serve as machine-checked and enforced APIs.
  }
  @item{ @bold{Ease:}
    The typed modules were the least effort to type.
    These could be the smallest modules or the ones with the fewest dependencies.
  }
  @item{ @bold{Necessity:}
    The typed modules are tightly-coupled to other typed modules or to typed
     library code.
    Such coupling between a typed module and an untyped module typically
     incurs a large run-time cost due to checking contracts.
    Typing both modules may improve performance.
  }
]
Over time, programs may become progressively more typed.
Consider a large untyped codebase in which a bug is traced to an
 module that was written years ago by a developer who is no longer with the project.
The programmer tasked with fixing the bug must first understand how the module
 is intended to work by reading documentation, unit tests, and the code; only
 then can the bug be fixed.
A side effect of this investigation is that the programmer uncovers the implicit
 type signatures of functions and datatypes in the module.
Typed Racket makes it easy to record these types.
The programmer
 only needs to write the inferred signatures, annotate imports from untyped
 modules, and change the @exact{\RktMeta{\#lang}} line from @exact{\RktMeta{racket}}
 to @exact{\RktMeta{typed/racket}}.
After converting, the module is now hardened against bugs due to type errors
 and documented with type annotations for future maintainers.

Introducing types in one module while leaving others untyped, however, may
 lead to significant performance overhead.
For example, Typed Racket developers have experienced pathologies including
 a 50% overhead in a commercial web server
 and 25x-50x slowdowns when using the standard math library.
One potential solution is to convert additional modules to Typed Racket.
Indeed,
 one user reported a speedup from 12 seconds to 1 millisecond after converting
 a script from Racket to Typed Racket.
But annotating a module is not guaranteed to improve performance and may introduce
 new type boundaries with even larger overhead.

As language designers, we must also remember that adding types is always
 a software engineering burden.
Even in the "campground" scenario@note{Golden rule of camping: always leave the campground cleaner than you found it.}
 outlined above, converting untyped code to
 Typed Racket is orthogonal to developers' primary goal of delivering a working
 software product.
@; Quote Matthew?
We must therefore strive to minimize the costs associated with gradual typing.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:overhead"]{The Source of Performance Overhead}

Boundaries between typed and untyped modules are the source of the performance costs
 in Typed Racket.
As a concrete example, consider a function for multiplying two complex numbers
 represented in polar form:

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

When this typed function is imported by an untyped module, we say it crosses a
 @emph{type boundary}.
During the import, the function's type signature is compiled into a
 higher-order contract @exact|{$\ctc{\RktMeta{(C C -> C)}}$}|
 to dynamically enforce correct usage.
The contract asserts that each call to @racket[complex-*] in untyped code
 is made with two arguments accepted by the @exact|{$\ctc{\RktMeta{C}}$}| contract;
 that is, two pairs of real numbers where the first component of each pair is
 non-negative.
Incorrect calls like @racket[(complex-* '(1 . 1) "NaN")]
 are detected immediately, before @racket[complex-*] is invoked, and
 the untyped code is blamed@~cite[ff-icfp-2002] for supplying arguments of the wrong type.

Correct blame is essential to Typed Racket's soundness guarantee, which states
 that typed code is never the cause of run-time type errors.
Rather, all type errors are either detected statically or dynamically caught
 and attributed to an untyped module.
Tracking blame and dynamically enforcing types can, however, be costly.
Each call to @racket[complex-*] requires six assertions to check and traverse both pairs.
This is relatively inexpensive, but folding @racket[complex-*] over a list of
 @math{n} complex numbers requires @math{3n + 1} assertions.
In brief, costs can quickly accumulate as the size of data
 and number of calls increases.

@; TODO example here, to show "surprising costs"?
@; - Derivatives of complex numbers
@; - Taylor series expansion
@; The program in @Figure-ref{TODO} underscores this point.
@; It is a script for (... tree / list ... complex ...)
@; When the script is typed, it runs in XXXms. Untyped, the same code takes
@;  YYYms to run.
@; TODO connect example to soundness?

Despite the potential overhead, it is crucial that each call to @racket[complex-*]
 is guarded against type errors.
Although inputs such as the string @racket{NaN} will cause an error
 when applied to @racket[+], ill-typed calls such as:
    @racketblock[(complex-* '(-1 . -1) '(-3 . 0))]
 fail @emph{silently}.
If we are lucky, the silent failure will trigger an error later in the program,
 but since the polar product of @racket['(-1 . -1)] and @racket['(-3 . 0)] is
 the well-typed complex number @racket['(3 . -1)] it is more likely that the
 program will compute an incorrect result and leave the programmer no clue as to
 where the error occurred.
Such are the dangers of committing ``moral turpitude''@~cite[r-ip-1983].


