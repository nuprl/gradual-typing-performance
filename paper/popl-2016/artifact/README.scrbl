#lang scribble/manual

@(require scribble/eval
          scriblib/autobib
          )
@(define (todo . xs) "")

@(define-cite ~cite citet generate-bibliography)

@(define (rtech pre) (tech pre #:doc '(lib "scribblings/reference.scrble")))

@title{Artifact: Is Sound Gradual Typing Dead?}

@(author (author+email "Asumu Takikawa" "asumu@ccs.neu.edu")
         (author+email "Dan Feltey" "dfeltey@ccs.neu.edu")
         (author+email "Ben Greenman" "types@ccs.neu.edu")
         (author+email "Max S. New" "maxsnew@ccs.neu.edu")
         (author+email "Jan Vitek" "j.vitek@ccs.neu.edu")
         (author+email "Matthias Felleisen" "matthias@ccs.neu.edu"))

This is a README file for the artifact that accompanies
"Is Sound Gradual Typing Dead?" in POPL 2016.

Our artifact is consists of a VM image that contains
@itemlist[
  @item{A distribution of the Racket programming language}
  @item{The benchmarking tools used to generate the data in the paper}
  @item{The source code for the paper}
  @item{The benchmarks used in the paper}
  @item{The benchmark data used in the paper}
 ]

The goals of this artifact are to
@itemlist[
  @item{Make the raw data we collected available for outside analysis}
  @item{Enable replication of our experimental evaluation}
 ]

Note that the larger benchmarks will take a very long time to run on a typical desktop computer.
For the time-constrained, see @secref{speed}.

@section{Setting up and installing the artifact}

The artifact is available as a virtual machine appliance for VirtualBox. If
you are already reading this README in the VM, feel free to ignore the
rest of this section.

To run the artifact image, open the given @tt{.ovf} file using the
@tt{File->Import Appliance} menu item. This will create a new VM
that can be launched after import. We recommend giving the VM at least
4GB of RAM.

The image is configured to automatically login to the @tt{artifact} user
account. The account has root privileges using @tt{sudo} without a password.
The password for the account is @tt{artifact}.

@section{Artifact Overview}
The relevant files are in @tt{/home/artifact/Desktop/}.
This directory contains
@itemlist[
  @item{@tt{README.html}: This page}
  @item{@tt{paper/}: A directory with the source code of the paper and the data used in the paper}
  @item{@tt{tools/}: A directory with the tools used to run the benchmarks and process the data generated}
  @item{@tt{benchmarks/}: A directory with the benchmarks used in the paper}
  @item{@tt{run.sh}: A script to run a particular benchmark}
  @item{@tt{run-all.sh}: A script to run all benchmarks in the @tt{benchmark/} directory. This may take as long as 2 months to complete.}
 ]

@section[#:tag "run"]{Running Benchmarks}
@todo{explain the ordering produced in the .rktd file}
To run a benchmark, use the @tt{run.sh} script.
For example, to run the @tt{morsecode} benchmark, run the command @tt{./run.sh benchmarks/morescode}.
This will produce two relevant files: @tt{benchmarks/morsecode.rktd} which contains the runtimes for
all runs performed and @tt{benchmarks/morsecode.png}, which is a contains the L-step N/M usable results as in Figure 4 of the paper.

The @tt{.rktd} file produced is a racket data file that contains a vector of lists of runtimes
that can easily be read into racket (see @secref{walkthrough} for examples).
Each list in the vector contains all the runtimes (in milliseconds) for a specific typed/untyped
configuration.
The ordering is a lexicographic in the typed/untyped bitstring which is formatted as follows.
For a program with @tt{n} modules, each configuration is assigned a length @tt{n} bitstring
where the @tt{i}th bit corresponds to the @tt{i}th module alphabetically.
@tt{1} denotes typed and @tt{0} denotes untyped.
For example, a program that has three modules: @tt{a.rkt}, @tt{b.rkt} and @tt{c.rkt} will produce
@tt{2^3=8} configurations and therefore a vector of 8 lists of runtimes.
The 5th runtime corresponds to the bitstring @tt{101} which means modules @tt{a.rkt} and @tt{c.rkt}
were using their typed versions and @tt{b.rkt} was using its untyped version.

The @tt{run-all.sh} script simply calls the @tt{run.sh} script on every benchmark in the @tt{benchmarks/} directory.

@section[#:tag "benchmarks"]{Benchmarks}

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

@subsection[#:tag "speed"]{Benchmarking Speed}

Many of the benchmarks take a very long time to run due to the high overheads of contract checking
and the large number of runs (exponential in the number of modules).

The two shortest-running benchmarks are
@itemlist[
  @item{mbta/}
  @item{morsecode/}
 ]

@Secref{walkthrough} contains a short-running benchmark as well.

@section[#:tag "paper"]{Paper}
The @tt{paper/} directory contains the data collected for all benchmarks in @tt{paper/data/}.
The directory also contains the source code for the paper.
The subdirectory @tt{paper/scripts/} contains files that may be useful for performing new analyses.
The racket files therein document their exports fairly well.

@section[#:tag "tools"]{Analysis Tools}

The @tt{tools/} directory contains more fine-grained scripts than @tt{run.sh} and @tt{run-all.sh}.

@tt{tools/run.rkt} will run a single benchmark, outputing an @tt{.rktd} file with the same basename
as the benchmark directory.

@tt{tools/view.rkt} will produce the LNM-plots (Figure 4 in the paper)
for any number of benchmarks
given a sequence of @tt{.rktd} files produced by @tt{tools/run.rkt}
It outputs it as @tt{output.png}.

@tt{tools/benchmark-util/data-lattice.rkt} will produce a picture summarizing the entire runtime lattice from a benchmark @tt{.rktd} file,
as in Figure 3 in the paper.
The black and white ovals correspond to the bitstrings explained in @secref{run}, where black denotes
typed and white untyped. The numbers are the ratio between the runtimes of the configuration and the
wholly untyped configuration.

@include-section{walkthrough.scrbl}
