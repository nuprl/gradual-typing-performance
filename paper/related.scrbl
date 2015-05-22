#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:rel"]{The State of the Related Work}

Papers about practical type systems for dynamic languages fall into two
categories: optional type systems that are unsound for typed-untyped
interoperation or sound gradual type systems. Most sound gradual type systems
do not come with a performance evaluation.

@section{Sound Gradual Type Systems}

Sound gradual typing has been applied to Python@~cite[vksb-dls-2014],
Smalltalk@~cite[acftd-scp-2013], and TypeScript@~cite[rsfbv-popl-2015
rnv-ecoop-2015]. Neither the Reticulated Python nor Gradualtalk systems
come with a comprehensive performance evaluation. However, Vitousek @etal
do note that ``Reticulated programs perform far worse than their
unchecked Python implementations'' and that their slowSHA program
exhibits a ``10x slowdown'' compared to Python@~cite[(in-bib vksb-dls-2014 ", pg. 54")].

Gradualtalk's evaluation is primarily qualitative, but Allende @etal
have investigated the overhead of several cast-insertion strategies
on Gradualtalk microbenchmarks@~cite[aft-dls-2013]. In another instance,
Allende @etal investigated the effect of confined gradual typing---an approach
in which the programmer can instruct the type system to avoid higher-order
wrapping where possible---in Gradualtalk on microbenchmarks@~cite[afgt-oopsla-2014].

Safe TypeScript comes with an evaluation on the Octane benchmarks ported to
TypeScript. Unlike our lattice-based approach, their evaluation essentially
only compares the performance of the fully untyped and fully typed programs.
Additionally, their micro gradual typing approach introduces overhead even in
``untyped'' programs. For Safe TypeScript, Rastogi @etal report slowdowns in
unannotated programs in a ``range from a factor of 2.4x (splay) to 72x
(crypto), with an average of 22x''@~cite[(in-bib rsfbv-popl-2015 ", pg. 178")].
On typed programs, the overhead is ``on average only 6.5%"@~cite[(in-bib rsfbv-popl-2015 ", pg. 178")].

StrongScript adds a sound type system on TypeScript but without any blame
tracking or higher-order wrappers for interoperation. Richards @etal use
the same microbenchmark suite as Safe TypeScript and compare the runtimes
of type-erased and fully-typed versions using their optimizing compiler.
They report that ``no benchmarks demonstrated slowdown outside of noise''
on the fully-typed versions@~cite[(in-bib rnv-ecoop-2015 ", pg. XXX")].
In our lattice terminology, the StrongScript comparison is essentially
between the top and bottom of the lattice. The performance of
intermediate states were not evaluated.

@section{Optional Type Systems}

Optional typing is an old idea whose roots can be traced as far back as
MACLISP, which allowed users to declare (unchecked) type
specifications@~cite[(in-bib moon-maclisp-1974 ", §14.2")]
in an otherwise untyped language.
The flavor of these annotations, and those in Lisp descendants such as Common
Lisp, differ from the contemporary view of optional types as statically-checked
annotations for software maintenance. In Lisp systems, these annotations are
used for compiler optimizations and dynamic checking.

Pluggable type systems are a closely related idea, exemplified by
Strongtalk@~cite[bg-oopsla-1993], and also fit into the unsound camp.  Recent
implementations, e.g. Papi @etal's work for Java@~cite[pacpe-issta-2008], layer
additional typed reasoning on top of existing typed languages rather than
untyped languages.

Optional type systems in the contemporary sense exist for
Clojure@~cite[bonnaire-sergeant-thesis-2012], Lua@~cite[mmi-dyla-2014],
Python@note[@url{http://mypy-lang.org/}], PHP@note[@url{http://hacklang.org/}],
and JavaScript@note[@url{http://flowtype.org}].  Since the type annotations in
these systems are unsound for typed-untyped interoperation, they incur no
runtime overhead from proxy wrapping or dynamic checks. As a result, our
performance analysis is inapplicable.
