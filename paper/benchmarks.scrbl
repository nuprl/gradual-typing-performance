#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:bm"]{The Benchmark Programs}

For our evaluation of Typed Racket, we strove to obtain benchmark
programs that are representative of code that users actually write. To this end,
most of the benchmarks are either based on third-party libraries or scripts sourced
from the original developer or from the Racket package repository.
A single benchmark is taken from an existing microbenchmark suite.

@section{Technical Overview}

The table in @figure-ref{"fig:bm"} lists and summarizes our 12 benchmark programs.
For each, we give an approximate measure of the program's size and a picture of
its module structure.

Size is measured by the number of modules and lines of code (LOC) in a program.
Crucially, the number of modules also determines the number of gradually-typed
variations we ran when testing the benchmark, as a program with @math{n} modules
can be gradually typed in @exact{$2^n$} possible variations.
Lines of code is less important for evaluating macro gradual typing,
but gives a sense of the overall complexity of each benchmark.
Moreover, the Typed LOC numbers are an upper bound on the annotations required
at any stage of gradual typing because each typed module in our experiment
fully annotates its import statements. In practice, only imports from untyped
modules require annotation.

The column labeled ``Other LOC'' measures the additional structure required to
run each project for all typed-untyped variations. This count includes project-wide
type definitions, typed interfaces to untyped libraries, and any @emph{typed adaptor
modules} we needed to add.

A typed adaptor module sits
between an untyped data definition module and the data's typed clients.
These adaptors create a uniform type signature for all typed modules to access
and reference.
This is crucial since Racket structure types
(record type definitions) are @emph{generative} in the sense that two distinct
definitions of the same structure type will define incompatible structures.

Finally, the module structure graphs show a dot for each module in the program
and draw an arrow from one module to another when the module at the arrow tail
imports definitions from the module at the arrow head.
The performance cost of gradual typing originates at boundaries between typed
and untyped modules, so it is interesting to compare the complexity of these
graphs with our experimental results in section@secref{sec:tr}.
This correlation, however, is a weak one because the module graphs only show
@emph{static} dependencies, whereas the runtime cost of each boundary depends
heavily on the frequency at which values flow across it.
@;;; to address this, we TODO

@section{Program Descriptions}

@; TODO a gentler intro
Here we briefly describe the functionality of each benchmark and note the dependencies
and adaptor modules required to run it.
Unless otherwise noted, the benchmarks rely only on core Racket libraries and
use no adaptor modules.

@parag{Sieve}
This program finds prime numbers using the Sieve of Eratosthenes and is our
smallest benchmark. It consists of two modules: a tiny streams library and a
script implementing the Sieve using streams.

@parag{Echo}
The echo server implements a simple network server/client pair and is a microbenchmark
originally used in the Computer Language Benchmarks Game@note{@url["http://benchmarksgame.alioth.debian.org/"]}.
Our adaptation divides the single-module program into four parts: client, server, shared constants, and main module.

@parag{Morse code}
The @tt{morse-code} script was adapted from a morse code training program.@note{@url["https://github.com/jbclements/morse-code-trainer"]}.
The original program would play a morse code audio clip, read the keyboard for
user input, and score the input based on its Levenshtein distance from the
correct answer. Our benchmark tests generating morse code strings and running the
Levenshtein algorithm on a list of frequently-used English words.

@parag{MBTA}
The @tt{mbta} program implements a server that asynchronously responds to
reachability queries about a model of Boston's public transit system.
The model is implemented using a third-party, untyped graph library.
This introduces a typed-untyped boundary even in the ``completely typed'' case.

@parag{Suffixtree}
The @tt{suffixtree} library implements a longest-common-substring algorithm
using Ukkonen's suffix tree algorithm. While the library has
minimal external dependencies, we need to add one adapter module for the
algorithm's internal data structures.

@parag{ZO Traversal}
The @tt{zo-traversal} script explores Racket bytecode structures (parsed from @tt{.zo} bytecode files)
and counts the frequency of AST nodes.
The script operates on the Racket compiler's untyped zo data structures.
Since these data structures are not natively supported in Typed Racket, even the
completely typed program incurs some dynamic overhead.

@parag{K-CFA}
The @tt{kfca} program is a simple implementation of control flow analysis for a
lambda calculus.
The language definitions and analysis are spread across seven modules, four of
which require adaptors because they introduce new datatypes.

@parag{Snake}
This program is based on a contract verification benchmark by Nguyên @|etal|@~cite[nthvh-icfp-2014].
It implements a game where a growing snake tries to eat apples while avoiding walls and its own tail.
Our benchmark, like Nguyên's, runs a pre-recorded history of moves altering the game state and does not display a GUI.
We use one adaptor module to represent the game data types, but otherwise the program is self-contained.

@parag{Synth}
The @tt{synth} benchmark is a sound synthesis example from St-Amour @|etal|'s work on
feature-specific profiling@~cite[saf-cc-2015].
The program consists of nine modules, half of which are from Typed Racket's array library.
In order to run these library modules in all typed-untyped variations we created an adaptor module
for the underlying array data structure.

@parag{Tetris}
This program is taken from the same benchmark suite as @tt{snake} and implements the eponymous game.
Like @tt{snake}, the benchmark runs a pre-recorded set of moves and required one
adaptor module.

@parag{Gregor}
This benchmark contains thirteen modules and stress-tests a date and time library.
The original library uses the @racketmodname[racket/generic]
library for ad-hoc polymorphism that is not supported by Typed Racket. We
get around this limitation by monomorphizing the code and removing @tt{gregor}'s
string parsing component.
The benchmark uses two adaptor modules and relies on a small, untyped library for
acquiring data on local times.

@parag{Quad}
The @tt{quad} project is an experimental typesetting library.
It depends on an external constraint satisfaction solver
library (to divide lines of text across multiple columns) and uses two adaptor modules.

@;; FIXME: remove figure caption rule
@figure*["fig:bm" "The software characteristics of the benchmarks"
@exact|{
\newcommand{\yespycket}{$\CIRCLE$}
\newcommand{\maybepycket}{$\RIGHTcircle$}
\newcommand{\nopycket}{$\Circle$}
\begin{tabular}[t]{lrrrrll}
\toprule
Project name          & \# Modules & Typed LOC & Untyped LOC & Other LOC & Module structure        \\
\midrule
%% FIXME: for Sieve is the placeholder.rkt module really needed? (1 LOC)
%% No, definitely not. It was only there to commit the base/ folder to git
%% (without it, you'd have to manually create the directory after cloning git to run sieve)
\tt{sieve}            & 2          & 87        & 69          & 0         & \pict{sieve}      \\
\tt{echo}             & 4          & 89        & 70          & 0         & \pict{echo}       \\
\tt{morse-code}       & 4          & 587       & 532         & 0         & \pict{morsecode}  \\
\tt{mbta}             & 4          & 578       & 532         & 89        & \pict{mbta}       \\
\tt{zo-traversal}     & 5          & 2121      & 1901        & 214       & \pict{zordoz}     \\
\tt{suffixtree}       & 6          & 945       & 866         & 40        & \pict{suffixtree} \\
\tt{kcfa}             & 7          & 401       & 397         & 257       & \pict{kcfa}       \\
\tt{snake}            & 8          & 276       & 214         & 27        & \pict{snake}      \\
\tt{synth}            & 9          & 1112      & 964         & 33        & \pict{funkytown}  \\
\tt{tetris}           & 9          & 575       & 457         & 38        & \pict{tetris}     \\
\tt{gregor}           & 13         & 1574      & 1455        & 103       & \pict{gregor}     \\
\tt{quad}             & 16         & 7702      & 7406        & 241       & \pict{quad}       \\
\bottomrule
\end{tabular}
}|
]
