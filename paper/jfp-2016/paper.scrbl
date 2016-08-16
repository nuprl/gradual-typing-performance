#lang scribble/jfp

@require["common.rkt" "benchmark.rkt" "util.rkt"]

@title{How to Evaluate the Performance of Gradual Type Systems}

@((author/short "Greenman, Takikawa, New, Feltey, Findler, Vitek, Felleisen")
  "BEN GREENMAN" (affiliation-mark "1")
  " ASUMU TAKIKAWA" (affiliation-mark "1")
  " MAX S. NEW" (affiliation-mark "1")
  " DANIEL FELTEY" (affiliation-mark "2")
  " ROBERT BRUCE FINDLER" (affiliation-mark "2")
  " JAN VITEK" (affiliation-mark "1")
  "and MATTHIAS FELLEISEN" (affiliation-mark "1")
  @(affiliation
    "Northeastern University" (affiliation-mark "1")
    (affiliation-sep)
    "Northwestern University" (affiliation-mark "2")))

@abstract{
  @; TODO needs work
  @; - "however"   --> cost of the guarantee?
  @; - "it is unclear"
  Every sound gradual type system guarantees that untyped components of a program can never break the type systems' static guarantees.
  The literature on gradual typing is, however, remarkably silent about the @emph{performance} of programs that mix typed and untyped components.
  It moreover lacks rigorous methods to quantify the overall performance of a gradual type system
   and to compare the performance of two gradual type systems.
  Clients therefore cannot assess whether a particular gradual type system will be practical given their performance requirements.
  Furthermore, implementors cannot measure the net effect of changes to a gradually typed language.

  This paper presents a method for evaluating the performance characteristics of gradual type systems.
  The method quantifies both the
   @emph{absolute performance} of a gradual type system on representative programs
   and the @emph{relative performance} of two gradual type systems for a fixed base language.
  Our validation of the method includes a comprehensive evaluation of
   @integer->word[(*NUM-BENCHMARKS*)] functional and object oriented benchmark
   programs on @integer->word[(length (*RKT-VERSIONS*))] versions of Typed Racket.
}

@include-section{intro.scrbl}
@include-section{story.scrbl}
@include-section{framework.scrbl}
@include-section{typed-racket.scrbl}
@include-section{threats.scrbl}
@include-section{devils.scrbl}
@;@include-section{experience.scrbl}
@;@include-section{flavors.scrbl}
@include-section{conclusion.scrbl}

@generate-bibliography[]
