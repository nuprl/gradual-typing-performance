#lang scribble/base

@require[
  "common.rkt"
  "typed-racket.rkt"
 (except-in gtp-summarize/lnm-parameters defparam)
]


@profile-point{sec:devils}
@title[#:tag "sec:devils"]{The Devil's Contracts}
@; -- AKA "Four contracts of the apocalypse"

@; TODO better intro, urgently
Many of our high-overhead benchmarks exhibit similar pathologies.
Here are some of the most interesting.

@; Of course contracts and boundary structure make it slow.
@; But some are worse than others. Or at least more interesting.

We hope that language designers manage to reduce the overhead caused by
 these pathological cases.
But in the worst case, these may serve as anti-patterns for performance-minded
 applications.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:frequency"]{Highway Contracts}
@; -- AKA high-frequency


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:types"]{Iceberg Contracts}
@; -- AKA unexpectedly large

@; Quad

@; avoiding chaperones
@; kcfa hashtable?
@; john clements or can I be less lazy?

@; future work: cost model for contracts?


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:boundary"]{Tunnelling Contracts}
@; -- AKA surprise boundaries

@; - choice of, synth/suffixtree
@; - macro-bending synth/suffixtree



@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:wrapping"]{Duplicate Contracts}
@; -- AKA repeated wrapping

@; functions in zombie
@; objects in forth,fsm(,acquire)
@; classes in dungeon?

@; ENCAPSULATION
@; acquire,take5 vs fsm,forth
@; better to do stateful than functional OO


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:library"]{Ecological Contracts}
@; -- AKA library

@; mbta, zordoz
@; dungeon (removed)

@; Need to convert the ecosystem

