#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:rel"]{The Sad State of Related Work}

Papers about practical type systems for dynamic languages fall into two
categories: @emph{optional type systems} which are unsound for typed-untyped
interoperation or sound gradual type systems. Most sound gradual type systems
do not come with a performance evaluation.

@section{Sound Gradual Type Systems}

Sound gradual typing has been applied to Python@~cite[vksb-dls-2014],
Smalltalk@~cite[acftd-scp-2013], TypeScript@~cite[rsfbv-popl-2015
rnv-ecoop-2015]. Neither the Reticulated Python nor Gradualtalk systems
come with a comprehensive performance evaluation. However, Vitousek @etal
do note that ``Reticulated programs perform far worse than their
unchecked Python implementations'' and that their slowSHA program
exhibits a ``10x slowdown'' compared to Python@~cite[(in-bib vksb-dls-2014 ", pg. 54")].
Gradualtalk's evaluation is primarily qualitative, but Allende @etal
have investigated the overhead of several cast-insertion strategies
on Gradualtalk microbenchmarks@~cite[aft-dls-2013].

Safe TypeScript comes with an evaluation on the Octane benchmarks ported to
TypeScript. Unlike our lattice-based approach, their evaluation essentially
only compares the performance of the fully untyped and fully typed programs.
Unlike our experience with macro gradual typing, micro gradual typing
introduces overhead even in ``untyped'' programs. For Safe TypeScript, Rastogi
@etal report performance in a ``range from a factor of 2.4x (splay) to 72x
(crypto), with an average of 22x''@~cite[(in-bib rsfbv-popl-2015 ", pg. 178")].

StrongScript adds a sound type system on TypeScript but without any blame
tracking or higher-order wrappers for interoperation. Richards @etal used
the same microbenchmark suite as Safe TypeScript and compared the runtimes
of type-erased and fully-typed versions using their optimizing compiler.
They reported that ``no benchmarks demonstrated slowdown outside of noise''
on the fully-typed versions@~cite[(in-bib rnv-ecoop-2015 ", pg. XXX")].
In our lattice terminology, the StrongScript comparison is essentially
between the top and bottom of the lattice. The performance of
intermediate states were not evaluated.

@section{Optional Type Systems}

@;; FIXME: need some additional citations here, e.g. pluggable types

Optional typing is an old idea whose roots can be traced as far back as
MACLISP, which allowed users to declare (unchecked) type specifications.  The
flavor of these annotations, and those in Lisp descendants such as Common Lisp,
differ from the contemporary view of optional types as statically-checked
annotations for software maintenance. In Lisp systems, these annotations are
used for compiler optimizations and dynamic checking.  Pluggable type systems
(e.g., for Java) are a closely related idea but as implemented typically layer
additional typed reasoning on top of an existing typed language.

Optional type systems in the contemporary sense exist for
Clojure@~cite[bonnaire-sergeant-thesis-2012], Lua@~cite[mmi-dyla-2014],
Python@note[@url{http://mypy-lang.org/}], PHP@note[@url{http://hacklang.org/}],
and JavaScript@note[@url{http://flowtype.org}].  Since the type annotations in
these systems are unsound for typed-untyped interoperation, they incur no
runtime overhead from proxy wrapping or dynamic checks. As a result, our
performance analysis is inapplicable.
