#lang scribble/base

@require["common.rkt" "typed-racket.rkt"]

@title[#:tag "sec:experience"]{Experience Report}

@figure*["fig:adaptor" "Inserting a type adaptor"
@exact|{
\input{fig-adaptor.tex}
}|
]

@section{Adaptor Modules}

At a high level, we generate all configurations for a benchmark program
 by manually writing typed and untyped versions and then generating all
 combinations of files for each version.
This would be sufficient but for the fact that Racket's structure type
 definitions are generative at type boundaries.
Consider the following structure definition from the @bm{gregor} benchmark:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(struct YearMonthDay [y m d])
))
@;%
Evaluating this statement introduces a new class of data structures
 characterized by a constructor (@racket[YearMonthDay]),
 a predicate (@racket[YearMonthDay?]),
 and three field selectors.
If this statement were evaluated a second time, a second class of data
 structures incompatible with the first would be defined.

If a structure-type definition is exported to other modules, a configuration
 may place the definition in an untyped module and its clients in typed
 modules.
If there are two typed clients, each will need to assign a type
 to the structure definition.
The straightforward way to assign types to untyped code is to replace imports
 in each typed module with a @racket[require/typed] statement.

@racketblock[
  (require/typed "untyped.rkt"
    [#:struct YearMonthDay (
      [y : Natural]
      [m : (U 'January 'February ...)]
      [d : Natural])])
]

Now, when these typed clients wish to exchange instances of these structure
 types, the type checker must prove that the static types match.
But each @racket[require/typed] generates a new type definition incompatible
 with the others.
Thus, even if the two clients use exactly the above declaration, they will
 have mutually incompatible @racket[YearMonthDay] types.

@Figure-ref{fig:adaptor} illuminates the problems with the left-hand diagram.
An export of a structure-type definition from the untyped module
 (star-shaped) to the two typed clients (black squares) ensures that the
 type checker cannot equate the two assigned static types.
The right-hand side of the figure explains the solution:
 we manually add a @emph{type adaptor module}.
Such adaptor modules are typed interfaces to untyped code.
The typed clients import structure-type definitions and the associated static
 types exclusively from the type adaptor, ensuring that only one canonical
 type is generated for each structure type.

Adaptor modules also reduce the number of type annotations needed at
 boundaries because all typed clients can reference a single point of
 control.@note{In our experimental setup, type adaptors are available to
 all configurations as library files.}
Therefore we have found adaptors useful whenever dealing with an untyped library.
Incidentally, the TypeScript community follows a very similar approach by
 using typed definition files (extension @tt{.d.ts}) to assign types to
 library code @todo{cite DefinitelyTyped}.


@; -----------------------------------------------------------------------------
@section{Failure to Launch}
@; Things we could not type

Occasionally, we hit a design that Typed Racket could not type check.
In all but one case these are known issues which future versions of Typed Racket
 will fix, but we note them here regardless.

@subsection{Multiple Return Values}
First, the non-issue.
The @bm{suffixtree} benchmark contained a function with type
 @racket[(All (A) ((-> A) -> A))] that executed a thunked computation
 and returned its result.
This type alone presented no trouble, but it was later applied to a thunk
 with two return values, expressed by the type
 @racket[(-> (Values Node Integer))].
Although the usage in @bm{suffixtree} happened to be safe, Typed Racket
 conservatively rejected the program because a different well-typed
 implementation might crash given a thunk with multiple values:

@racketblock[
  (: f (All (A) ((-> A) . -> . A)))
  (define (f thunk)
    (let ([r (thunk)])
      r))
]

@subsection{Unconventional Occurrence Types}
The first true issue we found was a limitation in Typed Racket's occurrence
 typing @todo{cite}.
In brief, occurrence typing refines union types when values flow through
 designated predicate functions.
For example, the following program type checks because occurrence typing
 knows that only values with type @racket[YearMonthDay] are recognized by the
 @racket[YearMonthDay?] predicate.

@racketblock[
  (: remove-missing (-> (Listof (U Null YearMonthDay))
                        (Listof YearMonthDay)))
  (define (remove-missing ymd*)
    (filter YearMonthDay? ymd*))
]

The following similar program ought to type check because partition returns two lists
 containing precisely the values that the @racket[YearMonthDay?] predicate
 passed and failed on, respectively.
Yet it fails because of subtleties in the implementation of
 negative type filters.@note{@url{https://github.com/racket/typed-racket/issues/138}}
@; For a challenge the reader should try to give a type for partition
Basic control-flow statements like @racket[if] explicitly handle the case where
 predicates fail, but there is no general-purpose mechanism.

@racketblock[
  (: get-missing (-> (Listof (U Null YearMonthDay))
                     (Listof Null)))
  (define (get-missing ymd*)
    (define-values (_ missing*)
                   (partition YearMonthDay? ymd*))
    missing*)
]

On the subject of occurrence typing, finding a semantics for occurrence typing
 structural objects is still an open problem.
The challenge is that @racket[is-a?] type tests succeed on untyped
 subclasses without enforcing that the subclass did not override the type
 of inherited methods.


@subsection{Type-to-Contract Limitations}
Not all Typed Racket types can be compiled into contracts.
This became an issue when typing @bm{synth} because polymorphic functions
 over polymorphic structs are one such type that have no corresponding contract.
Our solution was to remove all polymorphism from struct definitions
 in @bm{synth}.@note{This
 almost certainly improved performance, but we cannot make comparisons
 against an alternative that does not compile.}

@; TODO: why can't we compile this?
@; A. general issue with typed racket & impersonators
@; B. the seal changes how previously-defined functions would work


@subsection{Symbol-Dependent, Recursive Types}
@; - zombie symbol-based type (overloading arity)
The @bm{zombie} benchmark uses an encoding of objects as functions
 that dispatch on a symbol.
For example, a @racket[position] object with @racket[x] and @racket[y] coordinates
 would be implemented as a function that responds to the symbols
 @racket['x] and @racket['y] by returning thunks for the proper coordinate.

@racketblock[
  (define-type Position
    (Rec This
      (case-> (-> 'x (-> Integer))
              (-> 'y (-> Integer))
              (-> 'this (-> This)))))

  (: make-position (-> Integer Integer Position))
  (define (make-position x-coord y-coord)
    (lambda (sym)
      (let ([this (make-position x-coord y-coord)])
        (case (sym)
         [(x) (lambda () x-coord)]
         [(y) (lambda () y-coord)]
         [(this) (lambda () this)]
         [else (error "unknown symbol")]))))
]

This example does not compile in Typed Racket.
The issue is the recursive type.
Alone, the @racket[case->} is not an issue but the checker does not extend
 to recursive types.


@subsection{HTDP mixin}
@; - HTDP mixin polymorphism
@todo{Ask Asumu}


@; -----------------------------------------------------------------------------
@section{Burden of Type Annotations}

The downside of Typed Racket's expressive type system is that practical type
 inference is very difficult.
Aside from standard type constructors and primitive types, any value
 or untagged union of types are also valid types.
In some respects, macro gradual typing makes inference easier because at least
 there are no untyped, unannotated terms with typed code.
But without a catch-all @tt{Dynamic} type, users must give many type annotations
 before their programs compile.
The sheer number of required annotations is a common surprise to
 new users---nobody wants to annotate the return type of a for loop or
 the parameter of a simple anonymous function @todo{cite list}.
@; Also hard to keep annotations accurate. See racket/plot bug
@; https://github.com/racket/plot/pull/12
@; (This is a subtle bug that might be taken the wrong way)

Looking forward, with improvements to Typed Racket's inference it may become
 possible to infer types around an untyped module's interaction with typed code.
Annotating just the call site in an untyped module could potentially
 lower the cost of type boundaries.
The difference may be between crossing a boundary at the start of a loop
 rather than at each iteration.


@; -----------------------------------------------------------------------------
@section{Conversion Strategy}

Some general advice on converting from Racket to Typed Racket:
@itemlist[
  @item{
    @emph{Compile often}.
    Checking small parts of a file while leaving the rest commented out
     was useful for evaluating part of a module and determining
     where the type checker needed more annotations.
    In general Typed Racket does not stop at the first type error,
     so the compiler's output can get overwhelming.
    Also, giving intentionally wrong types like @racket[(Void -> Void)] to
     a function can trigger a suggestion from the compiler.
    This is especially useful when the suggested type is long, like
     @racket[(HashTable String (Listof Boolean))].
  }
  @item{
    Unit tests are the specification for untyped code @todo{cite furr/foster}.
    Comments go out of date and function definitions often leave their
     parameters under-constrained, but tests give precise inputs and
     more importantly exercise code paths.
  }
  @item{
    Prioritize data definitions.
    Having types for core data structures helps determine many other
     type constraints in a program.
    Whether or not the types are checked, a programmer should keep them in mind.

    Along the same lines, functions closely tied to a data structure
     should not communicate through a type boundary.
    If not, overhead like we experienced for @bm{suffixtree} and @bm{synth}
     should be expected.
  }
  @item{
    Consider using opaque types across boundaries.
    Opaque types hide the implementation of a type and leave run-time checking
     to a predicate.
    Normally this predicate is faster that verifying the structural properties
     of the type.
  }
  @item{
    When in doubt, prefer simple types like @racket[Integer] over more
     specific types like @racket[Natural].
    Being too eager with type constraints may cause more programming overhead
     in the form of assertions and other bookkeeping
     than it catches bugs.
    This is especially true when converting a project where the
     types are not certain ahead-of-time.
  }
  @item{
    Avoid forms like @racket[(all-defined-out)], both as a module writer and
     when converting from Racket to Typed Racket.
    As the writer, it makes the interface harder for future developers to
     understand.
    When converting, the lack of an explicit interface hints that the
     interface is unstable or frequently crossed.
    The former is less likely to impose an annotation burden and the latter
     a performance cost.
  }
]

In the event that a target group of modules is chosen to be typed,
 an interesting question is what order to convert them.
We found dependency-order (topological) the most useful, but going from
 most-to-least or least-to-most both had advantages.

For our benchmarking, most-to-least was typically better because we had a fixed
 set of inputs and needed specific types at boundaries.
Starting with the inputs helped determine other types and polymorphic
 instantiations.
Sadly needed to remove polymorphism at most boundaries to typecheck.

Least-to-most encourages more generic types; write these based on
 what the code does rather than how the code currently is used.

