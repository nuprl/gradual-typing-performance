#lang scribble/manual

@(require scribble/eval
          scriblib/autobib)
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

with the following caveats:
@itemlist[
  @item{The version of Racket included in the artifact is not exactly the same as the one used to create the data. However, the implementations of Typed Racket and the contract system are the same, so we expect little difference in the results}
  @item{The larger benchmarks will take a very long time to run on a typical desktop computer.}
 ]

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
@todo{fill in with the correct file name}
The relevant files are in @tt{/home/artifact/Desktop/gradual-typing-performance-x.x}.
This directory contains
@itemlist[
  @item{@tt{README.html}: This page}
  @item{@tt{paper/}: A directory with the source code of the paper}
  @item{@tt{tools/}: A directory with the tools used to run the benchmarks and process the data generated}
  @item{@tt{benchmarks/}: A directory with the benchmarks used in the paper}
  @item{@tt{run.sh}: A script to run a particular benchmark}
  @item{@tt{run-all.sh}: A script to run all benchmarks in the @tt{benchmark/} directory. This may take as long as 2 months to complete.}
 ]

@section{Running and Creating Benchmarks}
@todo{currently this is wrong: it outputs output.png, fix this before we send in the artifact.}
@todo{explain the ordering produced in the .rktd file}
To run a benchmark, use the @tt{run.sh} script.
For example, to run the @tt{morsecode} benchmark, run the command @tt{./run.sh benchmarks/morescode}.
This will produce two relevant files: @tt{benchmarks/morsecode.rktd} which contains the runtimes for
all runs performed and @tt{benchmarks/morsecode.png}, which is a contains the L-step N/M usable results as in Figure 4 of the paper.

The @tt{run-all.sh} script simply calls the @tt{run.sh} script on every benchmark in the @tt{benchmarks/} directory.

@section{Benchmarks}

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



@section{Benchmarking Tools}
