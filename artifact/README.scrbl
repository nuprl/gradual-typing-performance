#lang scribble/manual

@(require scribble/eval
          scriblib/autobib)

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
"Is Sound Gradual Typing Dead" in POPL 2016.

Our artifact consists of a set of evaluation projects using Typed Racket,
benchmarking infrastructure for those projects, and raw data from our
experimental runs.

The rest of this README consists of instructions on setting up and running
the artifact. All of the examples in this README are hyperlinked to the Racket
documentation which contains detailed information about all of the relevant
functions and syntactic forms.

@section{Setting up and installing the artifact}

Our artifact consists of three major parts:

@itemlist[
  @item{A distribution of the Racket programming language that comes with
        our implementation of gradual typing for classes}
  @item{The source code for evaluation projects, and}
  @item{Raw data from our benchmarking runs.}
]

The artifact is available as a virtual machine appliance for VirtualBox. If
you are already reading this README in the VM, feel free to ignore the
rest of this section.

To run the artifact image, open the given @tt{.ovf} file using the
@tt{File->Import Appliance} menu item. This will create a new VM
that can be launched after import. We recommend giving the VM at least
1GB of RAM.

The image is configured to automatically login to the @tt{artifact} user
account. The account has root privileges using @tt{sudo} without a password.
The password for the account is @tt{artifact}.
