#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:bm"]{The Benchmark Programs}

For our evaluation of Typed Racket, we strove to obtain benchmark
programs that are representative of code that users actually write. To this end,
most of the benchmarks are either based on third-party libraries or scripts sourced
from the original developer or from the Racket package repository.
A single benchmark is taken from an existing microbenchmark suite.

@section{Program descriptions}

This subsection details each of the benchmark programs in our evaluation. Each
description notes the number of modules, the shape of the module structure,
external dependencies of the program, and whether the program needed additional
tweaking to fit into the evaluation framework. The number of modules always
indicates how many modules are included in the lattice---i.e., are either
typed or untyped depending on the configuration. Adaptor
modules---explained below---or additional helper or data files that are always
untyped (or typed) are not included in the count.
@Figure-ref["fig:bm"] lists and summarizes the benchmarks.

Several of the projects need an ``adaptor module'' which sits
between a data definition module in the original program and its clients. The
adaptor is used in lieu of including the data definition module in the performance
lattice and ensures that both typed and untyped modules in the benchmark configurations
can use the same data definitions. This is crucial since Racket structure types
(record type definitions) are @emph{generative} in the sense that two distinct
definitions of the same structure type will define incompatible structures.
Without the adaptor, the typed and untyped modules may define incompatible data
structures.

@parag{Sieve}
This program finds prime numbers using the Sieve of Erastothones and is our
smallest benchmark with only two modules. Due to its simplicity, however, the
completely typed version of the program has only minimal dependencies on the core
Racket language. The module structure is also simple and only has a single
chain of dependencies.

@parag{Echo}
The echo server implements a simple network server/client pair and is a microbenchmark
originally used in the Computer Language Benchmarks Game@note{@url["http://benchmarksgame.alioth.debian.org/"]}.
It consists of four modules in a diamond shape in which the client and
server modules both depend on a shared constants module. This program also
only depends on Racket's core libraries.

@parag{Morse code}
The @tt{morse-code} script implements a morse code training program. Running the
script generates random sequences of words, translates them to morse code, and
computes the Levenshtein distance. The script contains four modules in a vee
shape (two chains of modules from a main module) and has minimal dependencies.

@parag{MBTA}
The @tt{mbta} program analyzes a graph representing a public transit route map.
It contains four modules in a single chain but also depends on a third-party
graph library. Since the graph library is untyped, this introduces a typed-untyped
boundary even in the ``completely typed'' case.

@parag{Suffixtree}
The @tt{suffixtree} library implements a longest-common-substring algorithm. The
implementation contains five modules in a single chain. While the library has
minimal external dependencies, we need to add in an adapter module for the
data definition module.

@parag{ZO Traversal}
The @tt{zo-traversal} script explores Racket bytecode structures and consists
of five modules. These modules are arranged in an almost diamond shape. The
script operates on the Racket compiler's untyped zo data structures. Since
these data structures are not natively supported in Typed Racket, even the
completely typed program incurs some dynamic overhead from using these structures.

@parag{KCFA}
The @tt{kfca} program is a small implementation of control flow analysis consisting
of seven modules arranged in a line or braid shape. This program requires
the use of four adaptor modules and contains one file which always
remains untyped.

@parag{Synth}
The @tt{synth} is a sound synthesis example from St-Amour @|etal|'s work on
feature-specific profiling@~cite[saf-cc-2015]. The program consists of nine modules. The original
example uses an external math library that we inline into this program
(by extracting modules from the library into this one). In addition, the
program also requires an adaptor module.

@parag{Tetris}
This benchmark program is based on a contract verification benchmark
by Nguyên @|etal|@~cite[nthvh-icfp-2014]. It implements the eponymous game
and consists of nine modules in a diamond shape. The addition of type annotations
requires an adaptor module.

@parag{Snake}
This program is taken from the same benchmark suite as @tt{tetris} and
consists of twelve modules in a diamond shape. Like @tt{tetris}, it also
requires an adaptor module.

@parag{Gregor}
This benchmark contains thirteen modules in a pyramid shape and stress-tests a
date and time library. The original library uses the @racketmodname[racket/generic]
library for ad-hoc polymorphism that is not supported by Typed Racket. We
get around this limitation by monomorphizing the code.

@parag{Quad}
The @tt{quad} project is an experimental document processing library consisting
of sixteen modules.

@;; FIXME: remove figure caption rule
@figure["fig:bm" "The software characteristics of the benchmarks"
@exact|{
\newcommand{\yespycket}{$\CIRCLE$}
\newcommand{\maybepycket}{$\RIGHTcircle$}
\newcommand{\nopycket}{$\Circle$}
\begin{tabular}[t]{lrll}
\toprule
Project name          & \# Modules & Module structure \\
\midrule
\tt{sieve}            & 2          & one chain        \\
\tt{echo}             & 4          & directed diamond \\
\tt{morse-code}       & 4          & vee              \\
\tt{mbta}             & 4          & one chain        \\
\tt{suffixtree}       & 5          & line             \\
\tt{zo-traversal}     & 5          & almost diamond   \\
\tt{kcfa}             & 7          & line, or braid   \\
\tt{synth}            & 9          & vine-like        \\
\tt{tetris}           & 9          & diamond          \\
\tt{snake}            & 12         & diamond          \\
\tt{gregor}           & 13         & pyramidic        \\
\tt{quad}             & 16         &                  \\
\bottomrule
\end{tabular}
}|
]
