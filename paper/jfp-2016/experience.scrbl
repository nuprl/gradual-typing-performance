#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:experience"]{Pragmatics of Typed Racket's Gradual Typing}

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


@section{Conversion Strategy}


@section{Failure to Launch}
@; Things we could not type
@; - HTDP mixin polymorphism
@; - generics
@; - polymorphic struct over boundary
@; - zombie symbol-based type (overloading arity)
@; - 


@section{Burden of Type Annotations}
@; macro-level gives some chance for type inference
@; But permissive type system fucks that
@; anyway the annotaions are a hard thing, common gotcha on users list
@; conversely, inference in parts of untyped code may help us optimize a region
@; compatible vs. covering types, #:samples bug vs. htdp mixin
