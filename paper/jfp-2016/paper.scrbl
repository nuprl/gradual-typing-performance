#lang scribble/jfp

@require["common.rkt" "benchmark.rkt" "util.rkt"]

@title{How to Evaluate the Performance of Gradual Type Systems}

@(@author/short{Greenman, Takikawa, New, Feltey, Findler, Vitek, Felleisen}
  "BEN GREENMAN" (affiliation-mark "1")
  " ASUMU TAKIKAWA" (affiliation-mark "1,3")
  " MAX S. NEW" (affiliation-mark "1")
  " DANIEL FELTEY" (affiliation-mark "1,2")
  " ROBERT BRUCE FINDLER" (affiliation-mark "2")
  " JAN VITEK" (affiliation-mark "1,4")
  "and MATTHIAS FELLEISEN" (affiliation-mark "1")
  @(affiliation
    "Northeastern University, Boston, Mass." (affiliation-mark "1")
    (affiliation-sep)
    "Northwestern University, Chicago, Ill." (affiliation-mark "2")
    (linebreak)
    "Igalia, San Francisco, Cal." (affiliation-mark "3")
    (affiliation-sep)
    "Czech Technical University, Prague, CZ " (affiliation-mark "4")
   ))

@abstract{
  A sound gradual type system ensures that untyped components of a program can never break the guarantees of statically typed components.
  This assurance relies on runtime checks, which in turn impose performance overhead in proportion to the frequency and nature of interaction between typed and untyped components.

  The literature on gradual typing lacks rigorous descriptions of methods for measuring the performance of gradual type systems.
  This gap has consequences for the implementors of gradual type systems and developers who use such systems.
  Without systematic evaluation of mixed-typed programs, implementors cannot precisely determine how improvements to a gradual type system affect performance.
  Developers cannot predict whether adding types to part of a program will significantly degrade (or improve) its performance.

  This paper presents the first method for evaluating the performance of sound gradual type systems.
  The method quantifies both the absolute performance of a gradual type system and the relative performance of two implementations of the same gradual type system.
  To validate the method, the paper reports on its application to @integer->word[(*NUM-BENCHMARKS*)] programs and @integer->word[(length (*RKT-VERSIONS*))] implementations of Typed Racket.
}

@include-section{intro.scrbl}
@include-section{story.scrbl}
@include-section{method.scrbl}
@include-section{benchmark.scrbl}
@include-section{typed-racket.scrbl}
@include-section{scale.scrbl}
@include-section{threats.scrbl}
@include-section{devils.scrbl}
@include-section{conclusion.scrbl}

@generate-bibliography[]

@include-section{appendix.scrbl}
