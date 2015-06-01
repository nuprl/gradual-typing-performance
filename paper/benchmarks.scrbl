#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:bm"]{The Benchmark Programs}

For our evaluation of Typed Racket, we strove to obtain benchmark
programs that are representative of code that users actually write. To this end,
most of the benchmarks are either based on third-party libraries or scripts sourced
from the original developer or from the Racket package repository.
A single benchmark is taken from an existing microbenchmark suite.
@Figure-ref["fig:bm"] lists and characterizes the benchmarks by the
number of modules in the program, the shape of the dependency structure
of the modules, and whether the benchmark successfully runs using Pycket.

The @tt{sieve} program finds prime numbers using the Sieve of Erastothones.
The @tt{echo} server implements a simple network server and is a task
originally used in the Computer Language Benchmarks Game@note{@url["http://benchmarksgame.alioth.debian.org/"]}.
The @tt{morse-code} script implements a
Morse Code training program. The @tt{mbta} program analyzes a graph
representing a public transit route map. The @tt{suffixtree} library
implements a longest-common-substring algorithm.
The @tt{zo-traversal} script explores Racket bytecode structures.
The @tt{kfca} program is a small implementation of control flow analysis.
The @tt{synth} is a sound synthesis example from St-Amour @|etal|'s work on
feature-specific profiling.
The @tt{tetris} and @tt{snake} benchmarks are games based on
contract verification benchmarks by Nguyên @|etal|@~cite[nthvh-icfp-2014].
The @tt{gregor} benchmark stress-tests a date and time handling library.
The @tt{quad} project is an experimental document processing library.

@;; FIXME: remove figure caption rule
@figure["fig:bm" "The software characteristics of the benchmarks"
@exact|{
\newcommand{\yespycket}{$\CIRCLE$}
\newcommand{\maybepycket}{$\RIGHTcircle$}
\newcommand{\nopycket}{$\Circle$}
\begin{tabular}[t]{lrll}
\toprule
Project name          & \# Modules & Module structure & Pycket \\
\midrule
\tt{sieve}            & 2          & one chain        & \yespycket \\
\tt{htdp}             & 4          & triangle         & \nopycket \\
\tt{echo}             & 4          & directed diamond & \nopycket \\
\tt{morse-code}       & 4          & vee              & \nopycket \\
\tt{mbta}             & 4          & one chain        & \nopycket \\
\tt{suffixtree}       & 5          & line             & \yespycket \\
\tt{zo-traversal}     & 5          & almost diamond   & \nopycket \\
\tt{kcfa}             & 7          & line, or braid   & \maybepycket \\
\tt{synth}            & 9          & vine-like        & \yespycket \\
\tt{tetris}           & 9          & diamond          & \nopycket \\
\tt{snake}            & 12         & diamond          & \yespycket \\
\tt{gregor}           & 13         & pyramidic        & \nopycket \\
\tt{quad}             & 16         &                  & \nopycket \\
\bottomrule
\end{tabular}
}|
]
