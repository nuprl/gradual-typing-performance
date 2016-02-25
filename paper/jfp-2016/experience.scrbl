#lang scribble/base

@require["common.rkt" "typed-racket.rkt"]

@title[#:tag "sec:experience"]{Experience Report}

@figure-here["fig:adaptor" "Inserting a type adaptor"
@exact|{
\input{fig-adaptor.tex}
}|
]

@section{Adaptor Modules}

A quirk in Racket's structure-type definitions calls for one twist to
an otherwise straightforward setup of benchmark configurations. Consider
the following structure-type definition from @tt{gregor}, one of the
benchmark programs: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(struct DateTime [date time jd])
))
@;%
Its evaluation introduces a new class of data structures via a constructor
(@racket[DateTime]), a predicate (@racket[DateTime?]), and a number of
selectors. A second evaluation creates a disjoint
class of structures, meaning the selectors for the
first class do not work on the second and vice versa.

If a structure-type definition is exported, a configuration may place the definition
in an untyped module and its clients into the typed portion of the program.
As explained below, importing a @racket[struct] demands that each client
assigns a type to the structure-type definition. Now, when these typed
clients wish to exchange instances of these structure types, the type
checker must prove that the static types match. But due to the above
quirk, the type system assigns generative static types to imported structure
types. Thus, even if the developers who annotate the two clients with types choose
the same names for the imported structure types, the two clients actually have
mutually incompatible static types.

@Figure-ref{fig:adaptor} illuminates the problems with the left-hand
diagram. An export of a structure-type definition from the untyped module
(star-shaped) to the two typed clients (black squares) ensures that the
type checker cannot equate the two assigned static types. The right-hand
side of the figure explains the solution. We manually add a @emph{type
adaptor module}. Such adaptor modules are specialized typed interfaces to
untyped code. The typed clients import structure-type definitions and the
associated static types exclusively from the type adaptor, ensuring that
only one canonical type is generated for each structure type.  Untyped
clients remain untouched and continue to use the original untyped file.

Adaptor modules also reduce the number of type annotations needed at
boundaries because all typed clients can reference a single point of
control.@note{In our experimental setup, type adaptors are available to
all configurations as library files.}  Therefore we expect type adaptor
modules to be of independent use to practitioners, rather than just a
synthetic byproduct of our setup.


@section{Failure to Launch}
@; Things we could not type
@; - HTDP mixin polymorphism
@; - generics
@; - polymorphic struct over boundary
@; - zombie symbol-based type (overloading arity)
@; - partition, no negative filters
@; - occurrence typing objects

Occasionally, we hit a design that Typed Racket could not type check.
In all but one case these are bugs which future versions of Typed Racket
 will fix, but we note them regardless.

@bm{suffixtree}
`(All (A) (-> (-> Void A) A))` was instantiated with a `(Values ...)` type.
The untyped program ran, but the typechecker rejected it.

A small example of necessary code changes was due to Typed Racket's weak type for `partition`.
Calling `partition` separates a list into two sub-lists, one with elements satisfying
a given predicate and the other with all the rest.
The types of both result lists should be refined through occurrence typing,
but the list of negatives is currently not refined (Typed Racket issue [#138](https://github.com/racket/typed-racket/issues/138)).
So we had to change the code to use two calls to `filter`, or else add a run-time assertion.


@section{Burden of Type Annotations}
@; macro-level gives some chance for type inference
@; But permissive type system fucks that
@; anyway the annotaions are a hard thing, common gotcha on users list
@; conversely, inference in parts of untyped code may help us optimize a region
@; compatible vs. covering types, #:samples bug vs. htdp mixin


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

