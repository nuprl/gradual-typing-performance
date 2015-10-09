#lang scribble/manual

@(require scribble/eval
          scriblib/autobib
          )

@title[#:tag "benchmarks"]{Benchmarks}

@section[#:tag "run"]{Running Benchmarks}

To run a benchmark, use the @tt{run.sh} script.
For example, to run the @tt{morsecode} benchmark, run the command @tt{./run.sh benchmarks/morescode}.
This will produce two relevant files: @tt{benchmarks/morsecode.rktd} which contains the runtimes for
all runs performed and @tt{benchmarks/morsecode.png}, which is a contains the L-step N/M usable results as in Figure 4 of the paper.

The @tt{run-all.sh} script simply calls the @tt{run.sh} script on every benchmark in the @tt{benchmarks/} directory.

Many of the benchmarks take a very long time to run due to the high overheads of contract checking
and the large number of runs (exponential in the number of modules).

The two shortest-running benchmarks are
@itemlist[
  @item{mbta/}
  @item{morsecode/}
 ]

@Secref{walkthrough} contains a short-running benchmark as well.

@section{Benchmark Structure}

A benchmark directory should contain at least 2 subdirectories: @tt{typed/} and @tt{untyped/}.
The two directories should contain the typed and untyped versions of each module in the benchmark program.

The directory may contain 2 other subdirectories: @tt{both/} and @tt{base/}.
Both contain files that should not change based on the typed/untyped configuration.
These include typed adaptor modules (as explained in section 3.1.1 in the paper),
external libraries not included in the configuration space,
and non-racket files used in the benchmark.
The only difference is where they are accessed they are placed relative to the configurations.
Files in the @tt{both/} directory will be copied into the directory of each configuration, whereas
files in the @tt{base/} directory will be copied into a single neighboring @tt{base/} directory,
i.e., they are accessible by the relative path @tt{../base/}.
Typed adaptors need to be in the @tt{both/} directory since they use @racket{require/typed/check} and
therefore have different behavior in different configurations.
In principle, everything in the @tt{base/} directory could be placed in the @tt{both/} directory instead
but it would be a large waste in storage space and runtime for large benchmarks.

