#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:bm"]{The Benchmark Programs}

For our evaluation of Typed Racket, we curated a suite of twelve programs.
They are representative of actual user code yet small enough so that
exhaustive exploration of the performance lattice remains tractable.  The
benchmarks are either based on third-party libraries or scripts sourced from
the original developer or the Racket package repository.

@section{Overview}

The table in @figure-ref{fig:bm} lists and summarizes our twelve benchmark
programs.  For each, we give an approximate measure of the program's size
and a diagram of its module structure.

Size is measured by the number of modules and lines of code (LOC) in a program.@note{We measured lines of code using the @hyperlink["http://www.dwheeler.com/sloccount/" "sloccount"] utility.}
Crucially, the number of modules also determines the number of gradually-typed
configurations to be run when testing the benchmark, as a program with @math{n} modules
can be gradually typed in @exact{$2^n$} possible configurations.
Lines of code is less important for evaluating macro gradual typing,
but gives a sense of the overall complexity of each benchmark.
Moreover, the Type Annotations LOC numbers are an upper bound on the annotations required
at any stage of gradual typing because each typed module in our experiment
fully annotates its import statements. In practice, only imports from untyped
modules require annotations.

The column labeled ``Other LOC'' measures the additional structure required
to run each project for all typed-untyped configurations. This count includes
project-wide type definitions, typed interfaces to untyped libraries, and
any so-called typed adaptor modules (see below).

Finally, the module structure graphs show a dot for each module in the program
and an arrow from one module to another when the module at the arrow tail
imports definitions from the module at the arrow head.
The performance cost of gradual typing originates at boundaries between typed
and untyped modules, so it is interesting to compare the complexity of these
graphs with our experimental results in section@secref{sec:tr}.
This correlation, however, is a weak one because the module graphs show only
@emph{static} dependencies, whereas the runtime cost of each boundary depends
heavily on the frequency at which values cross.

@;;; to address this, we TODO added a counter to each contract and ran the prog.


@;; FIXME: remove figure caption rule
@figure*["fig:bm" "The software characteristics of the benchmarks"
@exact|{
\newcommand{\yespycket}{$\CIRCLE$}
\newcommand{\maybepycket}{$\RIGHTcircle$}
\newcommand{\nopycket}{$\Circle$}
\begin{tabular}[t]{lrrrrll}
\toprule
Project name          & \# Modules & Untyped LOC & Type Ann. LOC & Other LOC & Module structure        \\
\midrule
\tt{sieve}            & 2          & 35          & 17            & 0         & \pict{sieve}      \\
\tt{morse-code}       & 4          & 216         & 29            & 0         & \pict{morsecode}  \\
\tt{mbta}             & 4          & 369         & 77            & 89        & \pict{mbta}       \\
\tt{zo-traversal}     & 5          & 1404        & 285           & 214       & \pict{zordoz}     \\
\tt{suffixtree}       & 6          & 545         & 125           & 40        & \pict{suffixtree} \\
\tt{lnm}              & 6          & 501         & 120           & 62        & \pict{lnm}        \\
\tt{kcfa}             & 7          & 248         & 47            & 141       & \pict{kcfa}       \\
\tt{snake}            & 8          & 161         & 50            & 27        & \pict{snake}      \\
\tt{tetris}           & 9          & 305         & 71            & 38        & \pict{tetris}     \\
\tt{synth}            & 10         & 837         & 142           & 33        & \pict{funkytown}  \\
\tt{gregor}           & 13         & 996         & 164           & 103       & \pict{gregor}     \\
\tt{quad}             & 16         & 6722        & 300           & 241       & \pict{quad}       \\
\bottomrule
\end{tabular}
}|
]


@subsection{Adaptor Modules}

Typed adaptor modules are a special case of typed interfaces to untyped code,
inserted when an untyped data definition and the data's typed clients are
part of the same configuration.
The adaptor is a typed module that exports annotated versions of all
bindings in the untyped data definition.
Typed clients then import exclusively from the typed adaptor, bypassing the
original data definition.
Untyped clients continue to use the untyped file.

@figure["fig:adaptor" "Inserting a typed adaptor"
@exact|{
\includegraphics[scale=0.25]{module-graphs/adaptor1.png}
\hspace{1cm}
\includegraphics[scale=0.25]{module-graphs/adaptor2.png}
}|
]

@Figure-ref{fig:adaptor} illustrates the basic problem that typed adaptors
solve.  The issue is that Racket structure types (record type definitions)
are @emph{generative}; each assignment of a type annotation to an untyped
structure creates a new ``black box'' definition.  This means that two
syntactically-identical type assignments to the same structure are
incompatible.  Using an adaptor ensures that only one canonical type is
generated for each structure, as illustrated in the right half of
@figure-ref{fig:adaptor}.

Strictly speaking, typed adaptor modules are not necessary.
It is possible to modify the design of imports for any given configuration so
that a single typed module declares and re-exports type annotations for untyped
data.
This alternative presents a non-trivial challenge, however,
when trying to synthesize the @math{2^n} gradually-typed configurations from
a fully-untyped and fully-typed version of each benchmark.

The layer of indirection provided by adaptors solves this issue and
reduces the number of type annotations needed at boundaries because all typed
clients can reference a single point of control.@note{In our experimental
framework, typed adaptors are available to all configurations as library files.}
Therefore we expect typed adaptor modules to be of independent use to
practitioners.


@section{Program Descriptions}

This section briefly describes each benchmark, noting the dependencies and required adaptor
modules.  Unless otherwise noted, the benchmarks rely
only on core Racket libraries and do not use adaptor modules.

@parag{Sieve}
This program finds prime numbers using the Sieve of Eratosthenes and is our
smallest benchmark. It consists of two modules: a tiny streams library and a
script implementing the Sieve using streams.

@parag{Morse code}
The @tt{morse-code} script is adapted from a morse code training program@note{@url["https://github.com/jbclements/morse-code-trainer"]}.
The original program plays a morse code audio clip, reads the keyboard for
user input, and scores the input based on its Levenshtein distance from the
correct answer. Our benchmark tests generating morse code strings and running the
Levenshtein algorithm on a list of frequently-used English words.

@parag{MBTA}
The @tt{mbta} program implements a server that asynchronously responds to
path queries about a graph representation of Boston's public transit system.
The graph representation is implemented using a third-party, untyped library.
The latter introduces a typed-untyped boundary even in the ``completely typed'' case.

@parag{ZO Traversal}
The @tt{zo-traversal} script provides a tool for exploring Racket bytecode structures
and counts the frequency of AST nodes.
The script operates on the Racket compiler's untyped zo data structures.
Since these data structures are not natively supported in Typed Racket, even the
completely typed program incurs some dynamic overhead.

@parag{Suffixtree}
The @tt{suffixtree} library implements a longest-common-substring algorithm
using Ukkonen's suffix tree algorithm. While the library has
minimal external dependencies, it calls for one adaptor module for the
algorithm's internal data structures.

@parag{L-NM}
This script analyzes the measurements included in this paper
and generates @figure-ref{fig:lnm1} and @figure-ref{fig:lnm1}.
Most of this benchmark's running time is spent generating figures using Typed Racket's @tt{plot} library, so the @emph{untyped} version of this progam is noticably less performant on large datasets.
This program relies on an untyped image rendering library and uses two adaptor modules.

@parag{K-CFA}
The @tt{kcfa} program implements a simple control flow analysis for a
lambda calculus.
The language definitions and analysis are spread across seven modules, four of
which require adaptors because they introduce new datatypes.

@parag{Snake} This program is based on a contract verification
benchmark@note{@url["https://github.com/philnguyen/soft-contract"]} by
Nguyên @|etal|@~cite[nthvh-icfp-2014].  It implements a game where a growing
and moving snake tries to eat apples while avoiding walls and its own tail.
Our benchmark, like Nguyên's, runs a pre-recorded history of moves altering
the game state and does not display a GUI.  We use one adaptor module to
represent the game data types, but otherwise the program is self-contained.

@parag{Tetris}
This program is taken from the same benchmark suite as @tt{snake}@~cite[nthvh-icfp-2014]
and implements the eponymous game.
Like @tt{snake}, the benchmark runs a pre-recorded set of moves. Using it here requires
one adaptor module.

@parag{Synth}
The @tt{synth} benchmark@note{@url["https://github.com/stamourv/synth"]}
is a sound synthesis example from St-Amour @|etal|'s work on
feature-specific profiling@~cite[saf-cc-2015].
The program consists of nine modules, half of which are from Typed Racket's array library.
In order to run these library modules in all typed-untyped configurations we created an adaptor module
for the underlying array data structure.

@parag{Gregor}
This benchmark consists of thirteen modules and stress-tests a date and time library.
The original library uses a
library for ad-hoc polymorphism that is not supported by Typed Racket. We
get around this limitation by monomorphizing the code and removing @tt{gregor}'s
string parsing component.
The benchmark uses two adaptor modules and relies on a small, untyped library for
acquiring data on local times.

@parag{Quad}
The @tt{quad} project implements a type-setting library.
It depends on an external constraint satisfaction solver
library (to divide lines of text across multiple columns) and uses two adaptor modules.
