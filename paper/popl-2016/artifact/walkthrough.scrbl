#lang scribble/manual
@(require scribble/eval
          scriblib/autobib
          )

@title[#:tag "walkthrough"]{Walkthrough: Creating and Analyzing a Benchmark Program}

To bring it all together we provide a walkthrough of creating a benchmark program,
running all configurations and analyzing the data.

@include-section{echo.scrbl}
@section{Running the Benchmark}

As before we can use @tt{./run.sh benchmarks/echo} to run our benchmark.
The raw data is now in @tt{benchmarks/echo.rktd}, if you read it you see the numbers are basically
all the same, so there's very little overhead from typing.

You can see the LNM graph at @tt{benchmarks/echo.png}. You'll just see a blue line at the top since
all configurations work here.

For another analysis, we can run @tt{racket tools/benchmark-util/data-lattice.rkt benchmarks/echo.rktd}, which will
show that no configurations have high overhead (all the numbers should be close to 1).

