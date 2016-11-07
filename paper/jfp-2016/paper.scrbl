#lang scribble/jfp

@require["common.rkt" "benchmark.rkt" "util.rkt"]

@title{How to Evaluate the Performance of Gradual Type Systems}

@(@author/short{Greenman, Takikawa, New, Feltey, Findler, Vitek, Felleisen}
  "BEN GREENMAN" (affiliation-mark "1")
  " ASUMU TAKIKAWA" (affiliation-mark "1")
  " MAX S. NEW" (affiliation-mark "1")
  " DANIEL FELTEY" (affiliation-mark "2")
  " ROBERT BRUCE FINDLER" (affiliation-mark "2")
  " JAN VITEK" (affiliation-mark "1")
  "and MATTHIAS FELLEISEN" (affiliation-mark "1")
  @(affiliation
    "Northeastern University, Boston, Mass." (affiliation-mark "1")
    (affiliation-sep)
    "Northwestern University, Chicago, Ill." (affiliation-mark "2")
   ))

@abstract{
  A sound gradual type system ensures that untyped components of a program can never break static type guarantees.
  This assurance requires dynamic type enforcement, which imposes runtime overhead.
  Surprisingly, the archival literature on gradual typing is silent about the @emph{performance} of programs that mix typed and untyped components.
  In particular, it lacks descriptions of rigorous methods for measuring the overall performance of a gradual type system
   and for comparing the performance of two gradual type systems.

  This gap has consequences for developers using gradual type systems and the implementors of such systems.
  Developers cannot assess whether a gradual type system will meet their performance requirements.
  Implementors cannot assess the effect of changes to a gradually typed language.

  This paper presents the first method for systematically evaluating the performance of gradual type systems.
  The method quantifies both the
   @emph{absolute performance} of a gradual type system on representative programs
   and the @emph{relative performance} of two implementations of the same gradual type system.
  Our validation includes a comprehensive evaluation of
   @integer->word[(*NUM-BENCHMARKS*)] functional and object-oriented benchmark
   programs on @integer->word[(length (*RKT-VERSIONS*))] versions of Typed Racket.
}

@include-section{intro.scrbl}
@include-section{story.scrbl}
@include-section{method.scrbl}
@include-section{benchmark.scrbl}
@include-section{typed-racket.scrbl}
@include-section{threats.scrbl}
@include-section{devils.scrbl}
@include-section{scale.scrbl}
@include-section{conclusion.scrbl}

@generate-bibliography[]

@include-section{appendix.scrbl}
