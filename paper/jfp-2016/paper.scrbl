#lang scribble/jfp

@require["common.rkt" "typed-racket.rkt"]

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
  Gradual typing opens a spectrum of possibilities;
   any program with @exact{$N$} locations that can be either typed or untyped
   can migrate to at least @exact{$2^N$} typed/untyped @emph{configurations}.
  Users will never explore this space, but performance evaluation of gradual
   type systems needs to summarize it, so that programmers know what to expect
   as they add typings were convenient.

  Second challenge: comparing two implementations of gradual typing for a
   fixed language.
  If one improves performance of fully typed programs, but always slows typed/untyped programs,
   is it really better?

  We propose methods for answering both questions: performance for users and
   comparisons for implementors.
  We evaluate these methods by applying them to Typed Racket.
  Our evaluation reports large overheads due to gradual typing, but on the
   other hand quantifies improvements between versions of Typed Racket.
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
