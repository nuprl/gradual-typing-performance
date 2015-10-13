#lang scribble/manual

@(require scribble/eval
          scriblib/autobib
          )

@title[#:tag "data"]{Data and Analysis}

@section{Data}
The data we present in the paper is in @tt{paper/data/}.
The @tt{.rktd} files contained therein are Racket data files in a simple S-expression format.
Each file contains a vector of lists of runtimes that can easily be read into Racket.
Each list in the vector contains all the runtimes (in milliseconds) for a specific typed/untyped
configuration.
The ordering is lexicographic in the typed/untyped bitstring which is formatted as follows.
For a program with @tt{n} modules, each configuration is assigned a length @tt{n} bitstring
where the @tt{i}th bit corresponds to the @tt{i}th module alphabetically.
@tt{1} denotes typed and @tt{0} denotes untyped.

As an example, a program that has three modules: @tt{a.rkt}, @tt{b.rkt} and @tt{c.rkt} will produce
@tt{2^3=8} configurations and therefore the @tt{.rktd} will have 8 lists of runtimes.
The bitstrings will be 3 bits longs, ordered from @tt{000}, @tt{001}, @tt{010}, etc. to @tt{111} in the data file.
Then the 3rd runtime corresponds to the bitstring @tt{011} which means modules @tt{b.rkt} and @tt{c.rkt}
were using their typed versions and @tt{a.rkt} was using its untyped version.

@section[#:tag "analysis"]{Analysis}

The subdirectory @tt{paper/scripts/} contains files that may be useful
for parsing the data and performing new analyses.
The Racket files therein document their exports fairly well.

The @tt{tools/} directory contains more high-level scripts.

@tt{tools/view.rkt} will produce the LNM-plots (Figure 4 in the paper)
for any number of benchmarks
given a sequence of @tt{.rktd} files produced by @tt{tools/run.rkt}.
Here is an example invocation (assuming the working directory is @tt{~/Desktop}):

  @nested[#:style 'inset]{@tt{racket tools/view.rkt paper/data/kcfa-2015-06-25T13:48:52.rktd}}

It outputs the plot as @tt{output.png} to the benchmarks directory.

@tt{tools/data-lattice.rkt} will produce a picture summarizing the entire runtime lattice from a benchmark @tt{.rktd} file,
as in Figure 3 in the paper. For example,

  @nested[#:style 'inset]{@tt{racket tools/data-lattice.rkt paper/data/kcfa-2015-06-25T13:48:52.rktd}}

The black and white ovals correspond to the bitstrings explained in @secref{run}, where black denotes
typed and white untyped. Each number is the ratio between the runtime of the configuration and the
wholly untyped configuration.
