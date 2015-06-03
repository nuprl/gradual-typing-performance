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


@; this is horrible English: 

@def{The @italic{performance lattice} of a @math{n} module system is the
 collection of all @math{2^n} system configurations that result from
 linking in typed and untyped variants of each module. The bottom of the
 lattice is the completely untyped configuration; the top is the completely
 typed one. Two configurations are one @italic{conversion step} apart if
 they differ in one module. If two configurations are one conversion step
 apart, the one with an additional typed module @italic{dominates} the
 other one.}

@def[#:term "typed/untyped ratio"]{The typed/untyped ratio of a performance
 lattice is time needed to run the completely typed variant divided by the
 completely untyped one.}

@def[#:term @list{@math{N}-ready}]{A configuration in a performance lattice is
 N-delivery-ready if its performance is no worse than a N% slowdown
 compared to the completely untyped configuration.}

@def[#:term @list{@math{(N,M)}-ready}]{A configuration in a performance
 lattice is (N,M)-ready if its performance is worse than a
 N% slowdown and no worse than an M% slowdown compared to
 the completely untyped configuration.}

@def[#:term @list{@math{L}-step @math{N}-ready, @math{L}-step
 @math{(M,N)}-ready}]{A performance lattice is L-step N-ready if a
 configuration that is not N-ready is at most L upward conversion steps
 away from an N-ready configurations. Similarly, a lattice is L-step
 (M,N)-ready if a configuration that is not (M,N)-ready is at most L upward
 steps away from one that is.}

                                                  

                                    
