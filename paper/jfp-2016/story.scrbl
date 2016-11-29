#lang scribble/base

@require["common.rkt" "benchmark.rkt" "util.rkt" racket/file
         (only-in pict ht-append pict-height vline)
         (only-in pict/code codeblock-pict) (only-in racket/string string-join)]

@profile-point{sec:story}
@title[#:tag "sec:story"]{Gradual Typing in Typed Racket}

Typed Racket@~cite[thf-popl-2008] is the oldest and most developed implementation of sound gradual typing.
It supports clients in both academia and industry.
Typed Racket attracts these clients because it accomodates the idioms of (untyped) Racket.
In fact, a major design goal of Typed Racket is that equipping a Racket program
 with types is only a matter of annotating identifiers and declaring recursive types.
Much of the underlying Racket program can remain the same, including code using
 variable-arity polymorphism@~cite[stf-esop-2009],
 first-class classes@~cite[tsdthf-oopsla-2012],
 delimited continuations@~cite[tsth-esop-2013],
 or contracts@~cite[l-mthesis-2016].
Finally, a typed module may incorporate definitions from a Racket module with a type-annotated import statement;
 conversely, a Racket module may use definitions from a Typed Racket module without knowledge that the providing module is typed.

@figure["fig:story:tr" "Gradual typing in Racket"
  @(let ([c1
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
         [c2
            @codeblock-pict[@string-join['(
            "#lang typed/racket"
            ""
            "(require/typed \"guess-game.rkt\""
            "  [play"
            "   (-> (-> Natural Boolean))])"
            ""
            "(: stubborn (-> Natural Natural))"
            "(define (stubborn n)"
            "  (define check-guess (play))"
            "  (if (check-guess n)"
            "    0"
            "    (+ 1 (stubborn n))))"
            ) "\n"]]])
  @ht-append[8 c1 @vline[2 (max (pict-height c1) (pict-height c2))] c2])
]

@Figure-ref{fig:story:tr} demonstrates Racket and Typed Racket with two small modules.
The Racket module on the left implements a simple guessing game with the function @racket[play].
Each call to @racket[play] generates a random number and returns a function that checks a given number against this random number.
The Typed Racket module on the right imports the function @racket[play] and defines a simple AI to interact with the guessing game.
The AI takes a natural number as input and calls @racket[play] until it wins with the given number.
Note that the typed module includes type annotations for both @racket[play] and the AI @racket[stubborn].
The type checker uses these annotations to search the right module for static type errors.
Typed Racket cannot guarantee that calls to @racket[play] return well-typed functions, but it assumes the static type is correct and inserts dynamic checks to monitor each function that @racket[play] returns.

Due to the close integration of Racket and Typed Racket, programmers frequently use both languages within a single application.
Furthermore, programmers often migrate Racket modules to Typed Racket as their application evolves.
In general one cannot predict why or how such incremental migrations happen, but here are some common motivations:
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
    Adding typed modules reduces @emph{friction} with typed libraries and clients.
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
Regarding the final point, there are two sources of so-called friction between typed and untyped code.
First is the above-mentioned requirement that typed clients must supply type annotations to use imports from an untyped library.
Maintainers of such libraries can instead provide a bridge module with the necessary annotations.
Second is the performance overhead of typed/untyped interaction, such as the overhead of dynamically enforcing the type annotation for @racket[play] in @figure-ref{fig:story:tr}.


@; -----------------------------------------------------------------------------
@section{The Costs of Incremental Typing}

Adding types to untyped code seems like an unequivocal benefit to software engineers.
For example, imagine a large application in which an error is traced to an untyped module that was written years ago.
In all likelihood, the programmer tasked with fixing the bug must recover the type specifications that the original developer had in mind---but did not write down.
Doing so involves studying the code and analyzing its unit tests.
This is a significant burden, but if the developer converts the module to Typed Racket, future maintainers will benefit from these explicit specifications.

Adding types to an untyped module is, however, a tradeoff.
Performing the type conversion may yield long-term benefits, but incurs immediate engineering costs.

The first cost is the burden of writing and maintaining type annotations.
In particular, Typed Racket is a @emph{macro-level}@exact{\,}@note{As opposed to @emph{micro-level} gradual typing@~cite[svcb-snapl-2015].} gradual type system.
Every expression within a Typed Racket module must pass the type checker.
Consequently, every recursive type in a module needs a declaration and every function parameter, class field, and induction variable needs a type annotation.

The second cost is the risk of introducing bugs during the conversion.
Typed Racket mitigates this risk by accomodating many Racket idioms,
 but occasionally programmers must refactor code to satisfy the type checker.
Refactoring can always spawn bugs.

The third cost is performance overhead due to typed/untyped interaction.
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
@;   @remark[]{
@bold{Remark:}
     Type boundary errors arise when type annotations impose new constraints that untyped code does not satisfy.
     Such errors may indicate latent bugs in the untyped code, but it is equally likely that the new type annotations are inaccurate.
     In other words, the slogan @exact{``well-typed programs can't be blamed''}@~cite[wf-esop-2009] misses half the point of gradual typing.
@;   }@;

Typed Racket implements the runtime check @ctc{$\tau$} for a first-order type @type{$\tau$} with a first-order predicate.
Every untyped value that flows into typed code at type @type{$\tau$} must satisfy the predicate.
To enforce a higher-order type, Typed Racket dynamically allocates a proxy to ensure that a value's future interactions conform with its static type@~cite[ff-icfp-2002 sthff-oopsla-2012].

For example, the Typed Racket function in @figure-ref{fig:complex-multiply} implements multiplication for polar-form complex numbers.@note{The example is adapted from Reynolds classic paper on types soundness@~cite[r-ip-1983]. In practice, Racket and Typed Racket have native support for complex numbers.}
If an untyped module imports this function, Typed Racket allocates a new proxy.
The proxy checks that every value which flows from the untyped module to @racket[polar-*] passes the predicate @ctc{C}.
@; If there are @math{n} calls to the function from untyped contexts, all @math{2n} values factor through the predicate.
These checks are indispensible because Typed Racket's static types operate at a higher level of abstraction than Racket's dynamic typing.
In particular, the tag checks performed by @racket[first] and @racket[*] do not enforce the @type{C} type.
Without the dynamic checks, the call
    @racket[(polar-* '(-1 0) '(-3 0))]
 would produce a well-typed complex number from two ill-typed inputs.
Racket's runtime would not detect this erroneous behavior, therefore the program would silently go wrong.
Such are the dangers of committing ``moral turpitude''@~cite[r-ip-1983].

Furthermore, even if a dynamic tag check uncovers a logical type error, debugging such errors in a higher-order functional language is often difficult.
Well-trained functional programmers follow John Hughes' advice and compose many small, re-usable functions to build a program@~cite[h-cj-1989].
Unfortunately, this means the root cause of a runtime exception is usually far removed from the point of logical failure.
@; Laziness does not pay in the context of type checking.

In contrast, sound gradual typing guarantees that typed code never executes a single instruction using ill-typed values.
Programmers can trust that every type annotation is a true statement because the gradual type system inserts runtime checks to remove any doubt.
These interposed checks furthermore detect type boundary errors as soon as possible.
If such an error occurs, the runtime enforcement system points the programmer to the relevant type annotation and supplies the incompatible value as a witness to the logical mistake.

@; These guarantees and improvements in developer productivity
@; do come at the price of runtime checks.
@; Performance evaluation quantifies these checks' cost,
@; lets developers quantify the tradeoff

    @figure["fig:complex-multiply" "Multiplication for polar form complex numbers"
      @codeblock-pict[@string-join['(
        "#lang typed/racket"
        ""
        "(provide polar-*)"
        ""
        "(define-type C (List Nonnegative-Real Real))"
        ";; C represents complex numbers as"
        ";;   ordered pairs (distance, radians)"
        ""
        "(: polar-* (C C -> C))"
        "(define (polar-* c1 c2)"
        "  (list (* (first  c1) (first  c2))"
        "        (+ (second c1) (second c2))))"
      ) "\n"]]
    ]

