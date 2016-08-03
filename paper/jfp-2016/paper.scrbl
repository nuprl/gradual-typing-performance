#lang scribble/jfp

@require["common.rkt" "typed-racket.rkt"]

@title{Performance Evaluation For Gradual Typing}

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
  Improvements to a programming language are traditionally evaluated by measuring
   performance before and after the improvement on a suite of benchmarks and
   computing the delta.
  This technique does not suffice to evaluate the performance of a gradual type
   system, as gradual typing offers a spectrum of possible ways for existing
   programs to evolve.
  Performance evaluation of a gradual type system, or any system for mixing
   languages within a program, @emph{must} consider all possibilities combinations
   the system opens to programmers.

  This paper is an extended report on our evaluation method for gradual
   typing@~cite[tfgnvf-popl-2016].
  In essence, the method is to report the overhead of all gradually typed
   @emph{configurations} of a program relative to the same program with no
   type annotations.
  We use the method to evaluate Typed Racket on a suite of @id[(count-benchmarks)]
   benchmark programs and to quantify improvements and regressions across
   versions of Typed Racket.
  The paper concludes with an in-depth discussion of the pathological slowdowns
   we observed and recommendations for implementors and users of gradual
   type systems.
}

@include-section{intro.scrbl}
@include-section{story.scrbl}
@include-section{framework.scrbl}
@include-section{typed-racket.scrbl}
@include-section{threats.scrbl}
@include-section{devils.scrbl}
@;@include-section{experience.scrbl}
@;@include-section{flavors.scrbl}
@;@include-section{conclusion.scrbl}

@generate-bibliography[]
