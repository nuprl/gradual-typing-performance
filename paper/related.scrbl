#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:rel"]{The State of the Related Work}

Gradual typing is a broad area teeming with both theoretical and practical
results.  This section focuses on implementations rather than formal models,
paying special attention to performance evaluation of gradual type
systems.

@section{Sound Gradual Type Systems}

Gradual typing has already been applied to a number of languages:
Python@~cite[vksb-dls-2014], Smalltalk@~cite[acftd-scp-2013],
Thorn@~cite[bfnorsvw-oopsla-2009] and TypeScript@~cite[rsfbv-popl-2015
rnv-ecoop-2015]. None of the projects report on conclusive studies of gradual typing's
impact on performance.

The authors of Reticulated Python recognized the performance issues of
gradual typing and designed the language to allow the exploration of efficient
cast mechanisms. However, Vitousek @etal note that ``Reticulated programs
perform far worse than their unchecked Python implementations'' and that
their @tt{slowSHA} program exhibits a ``10x slowdown'' compared to
Python@~cite[(in-bib vksb-dls-2014 ", pg. 54")].

Gradualtalk's evaluation is primarily qualitative, but Allende @etal have
investigated the overhead of several cast-insertion strategies on
Gradualtalk microbenchmarks and on two
macrobenchmarks@~cite[aft-dls-2013]. In addition, Allende
@|etal|@~cite[afgt-oopsla-2014] investigated the effect of confined gradual
typing---an approach in which the programmer can instruct the type system to
avoid higher-order wrapping where possible---in Gradualtalk on
microbenchmarks. These efforts evaluate the cost of specific features, but
do not represent the cost of the whole gradual typing process.

Safe TypeScript's evaluation is based on the TypeScript ports of the
Octane benchmarks. Unlike our lattice-based approach, it
compares only the performance of the fully untyped and fully typed programs.
Rastogi @etal report slowdowns in
unannotated programs in a ``range from a factor of 2.4x (splay) to 72x
(crypto), with an average of 22x''@~cite[(in-bib rsfbv-popl-2015 ", pg. 178")].
On fully typed programs, the overhead is ``on average only 6.5%"@~cite[(in-bib rsfbv-popl-2015 ", pg. 178")].

Thorn combines a sound type system with an optional type system, allowing
programmers to choose between so-called concrete types and like
types@~cite[bfnorsvw-oopsla-2009].  StrongScript follows Thorn's lead by
adding a sound type system (with a limited form of higher-order wrappers) to
TypeScript. Thorn has a minimal performance evaluation which shows that by
sprinkling a few type annotations over toy benchmarks, speed-ups between 3x
and 6x can be obtained@~cite[wnlov-popl-2010].  Richards @etal use the
same microbenchmark suite as Safe TypeScript and compare the runtimes of
type-erased and fully-typed versions using their optimizing compiler.  They
report ``no benchmarks demonstrated slowdown outside of noise'' (and up to 20%
speedups) on the fully-typed versions@~cite[(in-bib rnv-ecoop-2015 ",
pg. 97")]. In our lattice terminology, the StrongScript comparison reports
typed/untyped ratios only. The performance of
intermediate states are not evaluated.

@section{Optional Type Systems}

Optional typing can be traced as far back as
MACLISP, which allowed users to declare (unchecked) type
specifications@~cite[(in-bib moon-maclisp-1974 ", §14.2")]
in an otherwise untyped language.
The flavor of these annotations, and those in Lisp descendants such as Common
Lisp, differ from the contemporary view of optional types as statically-checked
annotations for software maintenance. In Lisp systems, these annotations are
used for compiler optimizations and dynamic checking.

Pluggable type systems are a closely related idea@~cite[bg-oopsla-1993 bracha-pluggable-types], and also belong to the unsound camp.  Recent
implementations, e.g. Papi @etal's work for Java@~cite[pacpe-issta-2008], layer
additional typed reasoning on top of existing typed languages rather than
untyped languages.

Contemporary optional type systems have been developed for
Clojure@~cite[bonnaire-sergeant-thesis-2012], Lua@~cite[mmi-dyla-2014],
Python,@note[@url{http://mypy-lang.org}]
PHP,@note[@url{http://hacklang.org}]
ActionScript,@note[@url{http://help.adobe.com/en_US/ActionScript/3.0_ProgrammingAS3/WS5b3ccc516d4fbf351e63e3d118a9b90204-7f8a.html}]
Dart,@note[@url{http://dartlang.org}] and
JavaScript@~cite[bat-ecoop-2014]. Since the
type annotations in these systems are unsound for typed-untyped
interoperation, they incur no runtime overhead from proxy wrapping or
dynamic checks. The lack of overheads obviates the need for a
performance evaluation such as the one in this paper.

Some publications have, however, investigated the performance impact of
optional typing with respect to compiler optimizations. Intuitively, one
would expect that a compiler could use these annotations as hints to
generate faster code. This intuition is borne out by Chang
@|etal|@~cite[cmscgbwf-dls-2011] who report significant speed-ups for
typed ActionScript code over untyped code. But one
should take such results with a pinch of salt as they are highly dependent
on the quality of the virtual machine used as the baseline.  Richards
@|etal|@~cite[rnv-ecoop-2015] report at most 20% speed up for fully typed
JavaScript. They ascribe this unimpressive result to the quality of the
optimizations implemented in V8. In other words, V8 is able to guess types
well enough that providing it with annotations does not help much.
