#lang scribble/base

@;; This is a test file taken from the documentation

@(require scriblib/autobib)

@(define-cite ~cite citet generate-bibliography)

@(define plt-tr1
   (make-bib
    #:title    "Reference: Racket"
    #:author   (authors "Matthew Flatt" "PLT")
    #:date     "2010"
    #:location (techrpt-location #:institution "PLT Inc."
                                 #:number "PLT-TR-2010-1")
    #:url      "http://racket-lang.org/tr1/"))

@title{Test file}

@section{Introduction}

Racket is fun@~cite[plt-tr1].

@(generate-bibliography)
