#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:rel"]{The State of the Related Work}

Gradual typing is a broad area teeming with both theoretical and practical
results.  In this section, we inspect only related work on
production systems rather than formal models or research
prototypes.
Publications about practical type systems for dynamic languages fall into two
categories: optional type systems that are unsound for typed-untyped
interoperation or sound gradual type systems.

@section{Sound Gradual Type Systems}

Few sound gradual type systems come with any performance evaluation efforts.
Sound gradual typing has been applied to Python@~cite[vksb-dls-2014],
Smalltalk@~cite[acftd-scp-2013], Thorn@~cite[bfnorsvw-oopsla-2009] and TypeScript@~cite[rsfbv-popl-2015
rnv-ecoop-2015]. Neither the Reticulated Python nor Gradualtalk systems
come with a comprehensive performance evaluation. However, Vitousek @etal
do note that ``Reticulated programs perform far worse than their
unchecked Python implementations'' and that their @tt{slowSHA} program
exhibits a ``10x slowdown'' compared to Python@~cite[(in-bib vksb-dls-2014 ", pg. 54")].

Gradualtalk's evaluation is primarily qualitative, but Allende @etal
have investigated the overhead of several cast-insertion strategies
on Gradualtalk microbenchmarks and on two macrobenchmarks@~cite[aft-dls-2013]. In addition,
Allende @|etal|@~cite[afgt-oopsla-2014] investigated the effect of confined gradual typing---an approach
in which the programmer can instruct the type system to avoid higher-order
wrapping where possible---in Gradualtalk on microbenchmarks. These efforts
evaluate the cost of specific features, but do not represent the
cost of the whole gradual typing process.

Safe TypeScript comes with an evaluation on the Octane benchmarks ported to
TypeScript. Unlike our lattice-based approach, their evaluation
compares only the performance of the fully untyped and fully typed programs.
For Safe TypeScript, Rastogi @etal report slowdowns in
unannotated programs in a ``range from a factor of 2.4x (splay) to 72x
(crypto), with an average of 22x''@~cite[(in-bib rsfbv-popl-2015 ", pg. 178")].
On fully typed programs, the overhead is ``on average only 6.5%"@~cite[(in-bib rsfbv-popl-2015 ", pg. 178")].

Thorn combines a sound type system with an optional type system, allowing
programmers to choose between so-called concrete types and like
types@~cite[bfnorsvw-oopsla-2009].  StrongScript follows Thorn's
lead by adding a sound type system to TypeScript but without any blame
tracking and very limited use of higher-order wrappers. Thorn had a minimal
performance evaluation which showed that by sprinkling a few type
annotations over toy benchmarks speed ups between 3x and 6x could be
obtained@~cite[wnlov-popl-2010].  Richards @etal use the same
microbenchmark suite as Safe TypeScript and compare the runtimes of
type-erased and fully-typed versions using their optimizing compiler.  They
report ``no benchmarks demonstrated slowdown outside of noise'' (and up 20%
speed ups) on the fully-typed versions@~cite[(in-bib rnv-ecoop-2015 ",
pg. 97")]. In our lattice terminology, the StrongScript comparison is
essentially between the top and bottom of the lattice. The performance of
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
Strongtalk@~cite[bg-oopsla-1993], and also belong to the unsound camp.  Recent
implementations, e.g. Papi @etal's work for Java@~cite[pacpe-issta-2008], layer
additional typed reasoning on top of existing typed languages rather than
untyped languages.

Optional type systems in the contemporary sense exist for
Clojure@~cite[bonnaire-sergeant-thesis-2012], Lua@~cite[mmi-dyla-2014],
Python@note[@url{http://mypy-lang.org/}], PHP@note[@url{http://hacklang.org/}],
and JavaScript@note[@url{http://flowtype.org}].  Since the type annotations in
these systems are unsound for typed-untyped interoperation, they incur no
runtime overhead from proxy wrapping or dynamic checks meaning there is no
need for a comprehensive evaluation such as the one suggested in this paper.

Previous publications have, however, investigated the performance impact of
optional typing with respect to compiler optimizations. For example, Chang
@|etal|@~cite[cmscgbwf-dls-2011] investigate the impact of optional type
annotations on JIT compilation.
@;; FIXME: expand
