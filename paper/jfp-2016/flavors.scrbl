#lang scribble/base

@; TODO
@; emphasize the theorem. The theorem is why sound is slow. Do you like the theorem?

@require["common.rkt"]

@title[#:tag "sec:flavors"]{Flavors of Gradual Typing}
The term @emph{gradual typing} was introduced by Siek & Taha to describe
 type systems that performed sound, static type checking only when function
 parameters were type annotated @todo{cite}.
Over the past decade, the restriction on function parameters has been lifted
 so that any sound, static type system that does not reject untyped code is
 a gradual type system.

Although there is variety among gradual type systems, @emph{gradual typing}
 is still a technical term; however, it is often applied to any language
 mixing some amount of static and dynamic type checking.
This section briefly summarizes the history of typed/untyped languages,
 shows where gradual typing entered the design space,
 and describes the various gradual type systems in use today.
Here, as in the introduction, we prefer to say ``sound gradual typing''
 to distinguish gradual typing (which is type sound) from optional type systems.

@; whatever man. Still bad but whatever


@section{Origins}

@; Dynamic ~ 1989
Work on gradual typing can be traced back to the type @tt{Dynamic} proposed
 for simply-typed and polymorphic languages @todo{abadi + mauny}.
Values of type @tt{Dynamic} have an unknown compile-time type, but flow into a
 program at runtime---perhaps by reading a file---with an explicit type tag.
To use a @tt{Dynamic} value, one must dispatch on the value of its tag.
Protected by a suitable guard, the untrusted value is safe to use in a typed
 program.

@; Soft ~ 1992
Soft typing @todo{cite} was an attempt to convert dynamic code into a statically
 typed language.
The key tenet of soft typing is that the type checker does not reject any
 programs it cannot prove correct.
Instead, the checker inserts runtime casts to validate properties that must
 hold in a safe execution.
The end result is improved static error detection and types that may be used
 to guide optimizations, all without imposing syntactic restrictions on the
 language user.
@; Theorems?
@; Where are we now?

@; Quasi-Static 1990
A third research direction combining static and dynamic types is Thatte's
 \emphquasi-static} typing @todo{cite}, which essentially removed the
 type tags from the previous work on type {\tt Dynamic}.
Instead of requiring the programmer to dispatch on the exact tag attached
 to values of unknown type, the quasi-static type system would infer or
 cast values automatically.
Only impossible type casts would result in a compile-time {\tt Dynamic} type error.
@; Kinda pointless paragraph, no?


@section{Optional Types}

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
Note that this form of optional typing is @emph{not} sound gradual typing
 because there is no guarantee that the runtime behavior of a program
 follows its type.

@figure["fig:ts-example" "Typed (left) and untyped (right) TypeScript code"
  @exact|{\input{fig-ts-example.tex}}|
]

@Figure-ref{fig:ts-example} gives two similar TypeScript programs.
The version on the left is fully typed; the type annotations are preceded by
 a @racket[:] character.
Any or all of these annotations may be removed to produce a partially-untyped
 version of the program.
Compiling any version of the program will produce the JavaScript code shown
 on the right---no matter what type annotations are present.
When the parameter to @racket[norm] is annotated, however, the TypeScript
 compiler will warn the programmer of a type error.

Emitting code regardless of known type errors is a design choice made by
 the TypeScript engineers, but it serves to demonstrate the language's
 type-unsoundess.
Types present in source code of optionally typed languages are
 not enforced at runtime.
Untyped code is free to call @racket[norm] or
 any other typed function with nonsense arguments.
At runtime, these calls might trigger a dynamic error, but even that is not guaranteed.
Illegal calls can also ``succeed'' silently and cause the program to produce
 incorrect results or diverge.

In practice unit testing should reveal dynamic type errors, but the fact remains
 that programmers using optional type systems cannot trust the type
 annotations for more than protection from mis-spellings or simple logical errors.
As a corollary, type-based optimizations are unsafe because they rely
 on information that may not be valid at runtime. @; memory error
On the other hand, erased types have zero run-time overhead.

@; Should TypeScript optimize with types? [devs say no]
@;   https://github.com/Microsoft/TypeScript/issues/1151
@; TypeScript, votes for soundness
@;   https://typescript.codeplex.com/discussions/428572
@; Why is Dart unsound?
@;   https://www.dartlang.org/support/faq.html#q-why-is-the-type-system-designed-to-be-unsound


@subsection{Pluggable Types}

Pluggable type systems are closely related to optional types, but
 require that type annotations have no impact on a program's dynamic behavior.
Type checking is completely independent of the underlying language runtime and
 functions only as a static analysis that rejects certain programs.
This design makes writing a new type system for an existing language
 straightforward.

The thesis of pluggable types is that a type system imposes extra restrictions
 on syntactically well-formed programs.
These restrictions may not be appropriate in all situations, hence the
 programmer ought to choose which type systems are suitable for a given project.

@; Java is pluggable https://docs.oracle.com/javase/tutorial/java/annotations/type_annotations.html


@section{Sound Gradual Typing}

@; Guarantees!
@; TC has dynamic component
@; Performance overhead

Sound gradual type systems statically check typed code for type errors,
 allow interaction between typed and untyped code, and additionally enforce
 the semantics of types at runtime.
The enforcement is what separates gradual types from optional types.
Whereas optional type systems erase the types during compilation,
 sound gradually typed languages convert static types into runtime assertions or
 casts.
When two typed parts of a program interact, these runtime checks are skipped.
But the boundary between typed and untyped code is always guarded.

@figure["fig:tr-example" "Typed and untyped Racket code"
@exact|{\input{fig-tr-example}}|
]

In short, a gradual type system includes both static and dynamic components.
Converting the typed program from @Figure-ref{fig:ts-example} into Typed Racket
 gives the code on the left side of @Figure-ref{fig:tr-example}.
If the call to @racket[norm] was part of the typed module, the program would
 fail to compile due to a static type error.
When the call is in an untyped module, as we have written it on the right
 half of @Figure-ref{fig:tr-example}, compilation
 succeeds and running the program raises an exception
 at the call site for @racket[norm] detailing the type error.
This type error is guaranteed by Typed Racket's soundness: run-time
 type errors are detected immediately and attributed to a
 responsible party @todo{cite}.

@;
Unfortunately, these runtime checks can have a large performance cost.
For small or simple values the cost is relatively low, though frequent checks
 will add up to a large slowdown.
More sophisticated values entail larger costs.
For instance, untyped lists require a linear-time check and untyped functions
 must be proxied to guarantee that each call returns well-typed output.
Conversely, if a mutable value flows from typed to untyped parts of the program,
 the value must be proxied to protect against updates that would change the
 value's type.
One would hope that the cost of dynamic checks is relatively small in practice,
 but gradually typed languages have reported slowdowns ranging from
 2x to 70x @todo{cite}.


@subsection{The Case for Soundness}

Given the obvious performance cost of type soundness, it seems more sensible
 to forget it.
Indeed, this is the approach taken by every optionally typed language
 developed in industry @todo{cite hack flow dart ts mypy}.
These languages use types to catch shallow yet common errors; build IDE tools
 like type-based autocompletion; and serve as checked documentation.

We believe that type systems---even gradual type systems---can offer much
 more to the working programmer.
First, sound types help developers reason about how code will execute
 in any context.
Once a function's domain is typed, there is no way it can be called
 with incorrect inputs.
Similarly, any data structure invariants that can be encoded in the type system
 are automatically preserved at runtime.
As the type system increases in scope and expressiveness, programmers
 can replace informal or asserted properties with stronger types.
 @; Example?

@; Eliminate errors
Sound types also eliminate a whole class of errors.
Milner's slogan "well typed programs do not go wrong" @todo{cite} is more
 formally a claim that runtime type errors are impossible in a language with
 a strong, static type system.
Gradually typed languages cannot prevent type errors from occurring, but
 they do accurately and immediately diagnose issues at runtime.
Therefore most of the work in debugging problems due to type errors is
 automated in a language with sound types.

@; Optimize
Lastly, sound types justify type-based optimizations.
Compilers may use type information to generate faster code.
@todo{cite morrissett/spj}
If optimizations outweight the cost of dynamic type checks, then sound
 types can provide a net peformance improvement.

@; Haskell https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/HscMain


@section{Micro vs. Macro Type Annotations}

Our examples have carefully used vague terms like "part" or "component"
 to refer to code that may be either typed or untyped.
Reason being, there are two schools of thought regarding the granularity at
 which typed and untyped code should be mixed.

@emph{Micro} gradual typing @todo{siek-taha} allows any @emph{variable} in the
 program be typed or untyped.
As @Figure-ref{fig:ts-example} demonstrates, class fields, function parameters,
 and function return types can be annotated or left untyped.
This gives enormous freedom adding types to a program.
Nearly all the languages mentioned above are examples of micro gradual typing
 @todo{cite}.

In contrast, @emph{macro} gradual typing @todo{th-f} requires the developer
 to declare each module as either typed or untyped.
Typed Racket is the only macro system we know of, which is somewhat surprising
 because macro gradual typing simplifies the implementation of the typechecker
 and leads to fewer boundaries between typed and untyped code.

@; More to say?
