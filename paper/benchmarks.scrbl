#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:bm"]{The Benchmark Programs}

For our evaluation of Typed Racket, we strove to obtain benchmark
programs that are representative of code that users actually write. To this end,
most of the benchmarks are either based on third-party libraries or scripts sourced
from the original developer or from the Racket package repository.
A single benchmark is taken from an existing microbenchmark suite.

@section{Program descriptions}
@; bg: I think this prose should just describe the table, and the adaptor
@;     then the parags would just give context and what the code does

This subsection details each of the benchmark programs in our evaluation. Each
description notes the number of modules, the shape of the module structure,
external dependencies of the program, and whether the program needed additional
tweaking to fit into the evaluation framework. The number of modules always
indicates how many modules are included in the lattice---i.e., are either
typed or untyped depending on the configuration. Adaptor
modules---explained below---or additional helper or data files that are always
untyped (or typed) are not included in the count.
@Figure-ref["fig:bm"] lists and summarizes the benchmarks.

Several of the projects need a typed ``adaptor module'' which sits
between a data definition module in the original program and its typed clients.
These adaptors create a uniform typed data definition for all variations in the
lattice. This is crucial since Racket structure types
(record type definitions) are @emph{generative} in the sense that two distinct
definitions of the same structure type will define incompatible structures.

In our experiment, untyped modules directly import definition files and typed
modules always import through an adaptor. Thus we incur a small, unnecessary
overhead when a typed module uses data originally defined in a typed module,
but overall this pattern made it feasible to test all variations with little
change to a project's original design.

@parag{Sieve}
This program finds prime numbers using the Sieve of Erastothones and is our
smallest benchmark. It consists of two modules---a tiny streams library and a
script implementing the Sieve using streams---and has minimal dependencies on
trusted core Racket libraries.

@parag{Echo}
The echo server implements a simple network server/client pair and is a microbenchmark
originally used in the Computer Language Benchmarks Game@note{@url["http://benchmarksgame.alioth.debian.org/"]}.
Our adaptation divides the single-module program into four parts: client, server, shared constants, and main endpoint.
This program also only depends on Racket's trusted core libraries.

@parag{Morse code}
The @tt{morse-code} script was adapted from a morse code training program.@note{@url["https://github.com/jbclements/morse-code-trainer"]}.
The original program would play a morse code audio clip, read the keyboard for
user input, and score the input based on its Levenshtein distance from the
correct answer. Our benchmark tests generating morse code strings and running the
Levenshtein algorithm on a list of frequently-used English words.
This program also has minimal dependencies.

@parag{MBTA}
The @tt{mbta} program implements a server that asynchronously responds to
reachability queries about a model of Boston's public transit system.
The model is implemented using a third-party, untyped graph library.
This introduces a typed-untyped boundary even in the ``completely typed'' case.

@parag{Suffixtree}
The @tt{suffixtree} library implements a longest-common-substring algorithm
using suffix trees. While the library has
minimal external dependencies, we need to add in an adapter module for the
data definition module.

@parag{ZO Traversal}
The @tt{zo-traversal} script explores Racket bytecode structures and counts the frequency of AST nodes.
The script operates on the Racket compiler's untyped zo data structures.
Since these data structures are not natively supported in Typed Racket, even the
completely typed program incurs some dynamic overhead from using these structures.

@parag{K-CFA}
The @tt{kfca} program is a simple implementation of control flow analysis consisting
of seven modules arranged in one chain.
This program requires four adaptor modules, but is otherwise self-contained.

@parag{Snake}
This program is based on a contract verification benchmark by Nguyên @|etal|@~cite[nthvh-icfp-2014].
It implements a game where a growing snake tries to eat apples while avoiding walls and its own tail.
Like @tt{tetris}, it also requires one adaptor module and runs a recorded history of game moves.

@parag{Synth}
The @tt{synth} benchmark is a sound synthesis example from St-Amour @|etal|'s work on
feature-specific profiling@~cite[saf-cc-2015].
The program consists of nine modules, half of which are from Typed Racket's array library.
In order to run these ex-library modules in all typed-untyped variations we created an adaptor module
for the underlying array data structure.

@parag{Tetris}
This program is taken from the same benchmark suite as @tt{snake} and implements the eponymous game.
An adaptor module wraps the record structure used to represent the game board.
Our benchmark runs a pre-recorded history
of moves altering the game state; it does not display a GUI.

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
