#lang scribble/sigplan @onecolumn 

@(require "common.rkt")

@(define numerals '(I II III IV V VI VII VIII))
@(define (authorinfo* name institution email)
   @authorinfo[
     (string-append "Dr. Double B. Reviewing, " 
       (begin0
          (symbol->string (car numerals))
          (set! numerals (cdr numerals))))
     (string-append (famous) " " "Famous University")
     "turing@award.com"])

@(define (famous)
   (define famous '("Very" "Somewhat" "Less" "In"))
   (list-ref famous (random (length famous))))

@authorinfo*["Asumu Takikawa" "Northeastern University" "asumu@ccs.neu.edu"]
@;authorinfo*["Spenser Bauman" "Indiana University" "sabauma@cs.indiana.edu"]
@authorinfo*["Daniel Feltey" "Northeastern University" "dfeltey@ccs.neu.edu"]
@authorinfo*["Ben Greenman" "Northeastern University" "types@ccs.neu.edu"]
@authorinfo*["Max S. New" "Northeastern University" "maxsnew@ccs.neu.edu"]
@;authorinfo*["Sam Tobin-Hochstadt" "Indiana University" "samth@cs.indiana.edu"]
@authorinfo*["Jan Vitek" "Northeastern University" "j.vitek@ccs.neu.edu"]
@authorinfo*["Matthias Felleisen" "Northeastern University" "matthias@ccs.neu.edu"]

@title{Is Sound Gradual Typing Dead?}

@abstract{Programmers have come to embrace dynamically typed languages for
 prototyping and delivering large and complex systems. When it comes to
 maintaining and evolving these systems, the lack of explicit static typing
 becomes a bottleneck. In response, researchers have explored the idea of
 gradually typed programming languages which allow the post-hoc addition of
 type annotations to software written in one of these ``untyped'' languages.
 Some of these new hybrid languages insert run-time checks at the boundary
 between typed and untyped code to establish type soundness for the overall
 system. With sound gradual typing programmers can rely on the language
 implementation to provide meaningful error messages when ``untyped'' code
 misbehaves.

 While most research on sound gradual typing has remained theoretical, the
 few emerging implementations incur performance overheads due to these
 checks. Indeed, none of the publications on this topic come with a
 comprehensive performance evaluation; a few report disastrous numbers on
 toy benchmarks. In response, this paper proposes a methodology for
 evaluating the performance of gradually typed programming languages.  The
 key is to explore the performance impact of adding type annotations to
 different parts of a software system. The methodology takes takes the idea
 of a gradual conversion from untyped to typed seriously and calls for
 measuring the performance of all possible conversions of a given untyped
 benchmark.  Finally the paper validates the proposed methodology using
 Typed Racket, a mature implementation of sound gradual typing, and a suite
 of real-world programs of various sizes and complexities.  Based on the
 results obtained in this study, the paper concludes that, given the state
 of current implementation technologies, sound gradual typing is
 dead. Conversely, it raises the question of how implementations could 
 reduce the overheads associated with ensuring soundness and how
 tools could be used to steer programmers clear from pathological cases.}

@include-section{intro.scrbl}
@include-section{framework.scrbl}
@include-section{benchmarks.scrbl}
@include-section{typed-racket.scrbl}
@;include-section{pycket.scrbl}
@include-section{death.scrbl}
@include-section{related.scrbl}
@include-section{conclusion.scrbl}

@generate-bibliography[]
