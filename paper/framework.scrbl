#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:fwk"]{Benchmarking Software Evolution}

We take the inspiration for our performance-evaluation approach from
 Takikawa @|etal|@~cite[tfdffthf-ecoop-2015] who present a formative evaluation of
 their gradual typing system for object-oriented Racket. 

This is a summative version of their evaluation, enriched with a systematic
inspection of lattice for evolutionary suitability 

 ---------------

describe the lattice in detail, abstractly and concretely, using one of our
example; make sure to discuss the ``completely typed'' issue/problem 

---------------

explain the questions we ask about the lattice: 

@bold{Question 1} how does the performance of the ``completely typed'' configuration
compare to the original, untyped configuration 

@bold{Question 2/a} how many of the lattice points experience no worse than an N%
slowdown compared to the untyped configuration? We call those N-@emph{delivery ready}.

@bold{Question 2/b} how many of the lattice points experience a slow-down of more
than N% but less than M% of the untyped configuration? We call those
M-@emph{evolutionary acceptable}.

@bold{Note 2/c} all other configurations are considered @emph{unacceptable}

@bold{Question 3} how many of the unacceptable configuration are within L
steps of delivery-ready or evolutionary-acceptable configuration? We call
those L-step acceptable.
