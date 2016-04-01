#lang scribble/jfp

@(require "common.rkt")

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
@bold{TBA}
@; Programmers have come to embrace dynamically-typed languages for
@; prototyping and delivering large and complex systems. When it comes to
@; maintaining and evolving these systems, the lack of explicit static typing
@; becomes a bottleneck. In response, researchers have explored the idea of
@; gradually-typed programming languages which allow the incremental addition of
@; type annotations to software written in one of these untyped languages.
@; Some of these new, hybrid languages insert run-time checks at the boundary
@; between typed and untyped code to establish type soundness for the overall
@; system. With sound gradual typing, programmers can rely on the language
@; implementation to provide meaningful error messages when type invariants
@; are violated.
@; While most research on sound gradual typing remains theoretical, the
@; few emerging implementations suffer from performance overheads due to these
@; checks. None of the publications on this topic comes with a
@; comprehensive performance evaluation. Worse, a few report disastrous numbers.
@;
@; In response, this paper proposes a method for
@; evaluating the performance of gradually-typed programming languages.
@; The method hinges on exploring the space of partial conversions from
@; untyped to typed. For each benchmark, the performance of the different
@; versions is reported in a synthetic metric that associates runtime overhead
@; to conversion effort.
@; The paper reports on the results of applying the method to
@; Typed Racket, a mature implementation of sound gradual typing, using a suite
@; of real-world programs of various sizes and complexities.  Based on these
@; results the paper concludes that, given the current state
@; of implementation technologies, sound gradual typing faces significant
@; challenges. Conversely, it raises the question of how implementations could 
@; reduce the overheads associated with soundness and how
@; tools could be used to steer programmers clear from pathological cases.
@;
@;@todo{6.2 vs 6.3 comparison}
}

@include-section{intro.scrbl}
@include-section{story.scrbl}
@include-section{framework.scrbl}
@include-section{typed-racket.scrbl}
@include-section{death.scrbl}
@include-section{experience.scrbl}
@include-section{flavors.scrbl}
@include-section{conclusion.scrbl}

@; @section{Acknowledgments}
@; - Stephen Chang for helping us adapt
@;
@;

@generate-bibliography[]
