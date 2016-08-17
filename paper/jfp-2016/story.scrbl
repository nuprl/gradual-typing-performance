#lang scribble/base

@require["common.rkt" "benchmark.rkt" "util.rkt"]
@(require racket/file benchmark-util/data-lattice)

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
Typed Racket programmers can import untyped values by writing an explicit type
 annotation for each import.

Converting a module between the Racket and Typed Racket languages is also straightforward.
The first line in every module declares its language, e.g. 
 @hash-lang[] @racket[racket] or @hash-lang[] @racket[typed/racket].
Typed Racket modules require annotations on function parameters, class fields,
 and mutable variables; otherwise, the syntax of Racket and Typed Racket is identical.
In particular, removing all type annotations and casts from a Typed Racket module yields
 a syntactically valid Racket module.

Given the close integration of Racket and Typed Racket, programmers tend to
 incrementally migrate untyped modules to Typed Racket.
Their reasons for migrating are diverse.
Below are a few common use cases, but in general we cannot predict why or how programmers add typed components.
@itemlist[
  @item{ @bold{Assurance:}
    The typechecker guards against common bugs, for example,
     forgetting to check for end-of-file symbols when reading from a port.
  }
  @item{ @bold{Compatibility:}
    Maintainers of untyped libraries provide annotations for library exports,
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
 throughout the module by reading the code and stepping though its unit tests.
This type recovery process might take hours or weeks.

After fixing the bug, the programmer can formally state these recovered specifications by converting the module to Typed Racket.
If the programmer takes this step, the module is now properly documented with type annotations for future maintainers.


@section{YOLO}
Maybe don't need subsection

Despite apparent benefits, must remember costs
- actually converting is not part of workflow
- orthogonal to primary goals
- changing code is possibility to add bugs
- type boundaries could kill ya


One crucial design goal of Typed Racket is that such conversions only require


If the developer


@todo{STOP}

Introducing types in one module while leaving others untyped, however,
 introduces type boundaries that may cause significant performance overhead.
For example, Typed Racket developers have experienced pathologies including
 a 50% overhead in a commercial web server
 and 25x-50x slowdowns when using the standard math library.
One potential solution is to convert additional modules to Typed Racket.
Indeed,
 one user reported a speedup from @|PFDS-BEFORE| to @|PFDS-AFTER| after converting
 a script from Racket to Typed Racket.
But annotating one additional module is not guaranteed to improve performance and may introduce
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

@; When a typed module imports definitions from an untyped module, programmers
@;  must supply type annotations.
@; The type system compiles these annotations to
@;  contracts@~cite[ff-icfp-2002] that ensure the untyped code matches its type specification.
@; Untyped modules may use definitions from typed modules without any annotation.
@; The type system generates contracts to protect the typed values from untyped contexts.
@; Put another way, every @emph{logical} type @exact|{$\RktMeta{T}$}| in Typed
@;  Racket has a @emph{computational} projection @exact|{$\ctc{\RktMeta{T}}$}|
@;  capable of dynamically checking whether an arbitrary value has type @exact|{$\RktMeta{T}$}|.

Boundaries between typed and untyped modules are the source of the performance costs
 in Typed Racket.
@todo{add abstract explanation}

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
Consequently, the contract immediately detects invalid arguments such as the string @racket{NaN}.

    @figure["fig:complex-multiply" "Multiplication for polar form complex numbers"
      @(begin
      #reader scribble/comment-reader
      @codeblock|{
        #lang typed/racket

        (define-type C (List Nonnegative-Real Real))
        ;; C = (Distance from origin, Radians)

        (: complex-* (C C -> C))
        (define (complex-* c1 c2)
          (list (* (first c1) (first c2))
                (+ (second c1) (second c2))))
      }|)
    ]

@; TODO 
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

Despite the potential overhead, Typed Racket must guard calls to @racket[complex-*]
 against type errors even though untyped Racket is a memory-safe
 programming language and will detect e.g. inputs like the string @racket{NaN}
 when they reach the @racket[+] function.
The danger is with ill-typed calls such as:
    @racketblock[(complex-* '(-1 . -1) '(-3 . 0))]
 which fail @emph{silently}.
If we are lucky, this silent failure triggers an error later in the program,
 but because the polar product of @racket['(-1 . -1)] and @racket['(-3 . 0)] is
 the well-typed complex number @racket['(3 . -1)], it is more likely that the
 program computes an incorrect result.
The programmer and the program's users receive no clue that an erroneous computation occurred.
Such are the dangers of committing ``moral turpitude''@~cite[r-ip-1983].

