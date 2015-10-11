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

The goals of this artifact are to
@itemlist[
  @item{Make the raw data we collected available for outside analysis}
  @item{Enable replication of our experimental evaluation}
 ]

For instructions on setting up the virtual machine, see @secref{vm}.

If you want to analyze the data presented in the paper, see @secref{data}.

If you want to analyze the benchmark programs or run the benchmarks yourself, see @secref{benchmarks} and possibly @secref{walkthrough}.

@section[#:tag "vm"]{Setting up and installing the artifact}

The artifact is available as a virtual machine appliance for VirtualBox and
can be downloaded at this URL:

  @nested[#:style 'inset]{@url["http://www.ccs.neu.edu/home/asumu/artifacts/popl-2016/artifact.tar"]}

The archive contains a @tt{.vmdk} and @tt{.ovf} file.

If you are already reading this README in the VM, feel free to ignore the
rest of this section.

To run the artifact image, open the given @tt{.ovf} file using the
@tt{File->Import Appliance} menu item. This will create a new VM
that can be launched after import. We recommend giving the VM at least
4GB of RAM if you want to run the analysis on the largest datasets. We
also recommend allocating at least two cores in the VirtualBox settings
for the VM.

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

See the following sections for more details on the contents of these directories
and how to use the included scripts.

@include-section{data-analysis.scrbl}
@include-section{benchmarks.scrbl}
@include-section{walkthrough.scrbl}
