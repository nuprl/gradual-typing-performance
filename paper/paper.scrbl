#lang scribble/sigplan @;onecolumn 

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
 prototyping and delivering complex systems. When it comes to maintaining
 and evolving these systems, the lack of explicit static typing turns into
 a bottleneck. In response, researchers have explored the idea of gradually
 typed programming languages, which allow the post-hoc addition of types to
 software written in these ``untyped'' languages. These hybrid languages
 insert run-time checks at the boundary between typed and untyped code to
 establish type soundness for the overall system. 

While most research on gradually typed programming languages has remained
 theoretical, the few emerging implementations seem to suffer from the cost
 of these run-time checks. Indeed, none of the publications on gradual
 typing come with a comprehensive evaluation; a few report disastrous
 numbers. In response, this paper proposes a framework for evaluating the
 performance of gradually typed programming languages. The framework takes
 the idea of a gradual conversion seriously and calls for measuring the
 performance of all possible conversions of some untyped benchmark. Finally
 the paper reports on the application of the framework to Typed Racket, the
 most advanced and sophisticated implementation of gradual typing. Based on
 these results, the paper concludes that, given current implementation
 technology, sound gradual typing is dead. Conversely, it raises the
 question of how to overcome this performance bottleneck or how to produce
 tools that help programmers avoid them.
}

@include-section{intro.scrbl}
@include-section{framework.scrbl}
@include-section{benchmarks.scrbl}
@include-section{typed-racket.scrbl}
@;include-section{pycket.scrbl}
@include-section{death.scrbl}
@include-section{related.scrbl}
@include-section{conclusion.scrbl}

@generate-bibliography[]
