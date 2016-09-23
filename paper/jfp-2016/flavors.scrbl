#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:flavors"]{Related Work}
The term @emph{gradual typing} was introduced by Siek & Taha to describe
 type systems that performed sound, static type checking only within functions
 whose parameters were type annotated @todo{cite}.
Over the past decade, the restriction on function parameters has been lifted
 so that any class field or variable declaration may or may not have a type
 annotation.
Crucially, these type systems provide an unknown (or dynamic) type called
 @tt{?} or @tt{Dyn} to reason about untyped values within a typed function.
Typing judgments coerce values from @tt{Dyn} at key points
 when constructing a type derivation and raise a cast error if the coercion fails.

The same year that Siek & Taha published their original paper, Tobin-Hochstadt
 and Felleisen described a system for combining typed and untyped @emph{modules}
 in a single program with higher-order contracts @todo{cite} enforcing the type system's
 invariants at runtime @todo{cite}.
Their type system was entirely conventional and did not require a dynamic type.
Untyped values were instead assigned a type when they crossed into typed
 code; the boundary at which the assignment happened would later be flagged
 if the untyped value did not act according to its type specification.
This approach has become known as @emph{macro} gradual typing, in contrast
 to Siek & Taha's @emph{micro} gradual typing.


@section{Optional Types}

Many languages combine static and dynamic type disciplines, but
 only a small minority of these are gradually typed.
That is, only a few guarantee the meaning of types at runtime.
The other languages are @emph{optionally typed}.

An optional type system lets programmers annotate untyped code, but makes no
 guarantee about how type annotations affect compilation or execution.
Compilers are free to reject ill-typed programs, perform type-based
 optimization, and/or completely ignore the types.

The first language with optional type annotations was
 MACLISP@~cite[(in-bib moon-maclisp-1974 ", §14.2")].
MACLISP annotations were not statically checked, but nonetheless some compilers
 would optimize based on the types @todo{cite}.
Many contemporary languages support or have extensions for optional types,
 including Common LISP, VB.NET, Python, and JavaScript
 @todo{cite}.
@;Common LISP
@;Dylan
@;Cecil
@;VB.NET
@;Bigloo
@;Clojure@~cite[bonnaire-sergeant-thesis-2012], Lua@~cite[mmi-dyla-2014],
@;Python,@note[@url{http://mypy-lang.org}]
@;PHP,@note[@url{http://hacklang.org}]
@;ActionScript,@note[@url{http://help.adobe.com/en_US/ActionScript/3.0_ProgrammingAS3/WS5b3ccc516d4fbf351e63e3d118a9b90204-7f8a.html}]
@;Dart@note[@url{http://dartlang.org}] and
@;JavaScript @~cite[bat-ecoop-2014]. @; Purescript
@;StrongTalk
@;BabyJ

The most common variety of optional type systems are strong, static type
 systems that accept untyped code @todo{cite TypeScript, Hack, etc}.
Typically, programmers using these languages can opt-in to static typing by
 writing type annotations.
The compiler statically checks all typed code for type errors and just accepts
 syntactically valid untyped code.
Note that this form of optional typing is @emph{not} gradual typing
 because there is no guarantee that the runtime behavior of a program
 follows its type.

@figure["fig:ts-example" "Typed (left) and untyped (right) TypeScript code"
  @exact|{\input{fig-ts-example.tex}}|
]

To illustrate, @Figure-ref{fig:ts-example} gives two similar TypeScript programs.
The version on the left is fully typed; the type annotations are preceded by
 a @racket[:] character.
Any or all of these annotations may be removed to produce a partially-untyped
 version of the program.
Compiling any version of the program will produce the JavaScript code shown
 on the right---no matter what type annotations are present.
When the parameter to @racket[norm] is annotated, however, the TypeScript
 compiler will warn the programmer of a type error at the call to @racket[norm].

Producing JavaScript code regardless of known type errors is a design choice made by
 the TypeScript engineers, but it serves to demonstrate the language's
 type-unsoundess.
Types present in source code of optionally typed languages are
 not enforced at runtime and
 this is a problem even in the absence of compile-time errors.
Untyped code is free to call @racket[norm] or
 any other typed function with nonsense arguments.
At runtime, these calls might trigger a dynamic error, but
 illegal calls can also ``succeed'' silently and cause the program to produce
 incorrect results or diverge.

In practice unit testing should reveal dynamic type errors, but the fact remains
 that programmers using optional type systems cannot trust the type
 annotations for more than protection from mis-spellings or simple logical errors.
As a corollary, type-based optimizations are unsafe because they rely
 on information that may not be valid at runtime. @; memory error
But you get what you pay for, and optional types are ``free'' in the sense
 that they impose zero runtime cost on a program.

@; Should TypeScript optimize with types? [devs say no]
@;   https://github.com/Microsoft/TypeScript/issues/1151
@; TypeScript, votes for soundness
@;   https://typescript.codeplex.com/discussions/428572
@; Why is Dart unsound?
@;   https://www.dartlang.org/support/faq.html#q-why-is-the-type-system-designed-to-be-unsound


