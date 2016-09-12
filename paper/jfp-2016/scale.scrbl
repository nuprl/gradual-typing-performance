#lang scribble/base

@require["common.rkt" "util.rkt" "benchmark.rkt"]

@profile-point{sec:scale}
@title[#:tag "sec:scale"]{Scaling the Method}

@; - take 1/8, try the optimistic/pessimistic sampling
@; - predictions? so far not working
@;   - at least profile the untyped
@; - prune the lattice, like
@;   - remove non-chaperone boundaries
@; - buy more machines, use a cluster

The fundamental limitation of our evaluation method is that it requires an exponential number of measurements.
Given one fixed type assignment for a program of @math{N} modules, there are @exact{$2^{N}$} configurations.
In general, if there are @math{M} ways to type each module, then there are @exact{$M^N$} configurations.

To date, we have explored three alternatives for scaling the method.
@; Our success with these alternatives has been mixed.
We invite other researchers in the field, especially those working with micro-level gradual type systems, to further pursue the alternatives described here or to suggest novel techniques for evaluating gradual type systems at scale.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:scale:par"]{Parallelizing the Experiment}
@; - embarrasingly parallel
@; - only delays the problem
@; - memory contention locally
@; - outliers on karst


@; -----------------------------------------------------------------------------
@section[#:tag "sec:scale:rnd"]{Sampling the Lattice}
@; - optimistic, pessimistic, 5 randoms
@; - sample size 1/8 1/16


@; -----------------------------------------------------------------------------
@section[#:tag "sec:scale:pre"]{Predicting Performance}
@; - software product lines
@; - maxnew method, verymixed success
@;   - might as well use mean, or use untyped
@;   - challenge to isolate boundaries


@; @; -----------------------------------------------------------------------------
@; @section[#:tag "sec:scale:dev"]{Ask Developers for Help}
@; - ask developers how they would add types
@;   - perhaps to their own project
@; - check the git history of a Typed Racket project, see how conversion happened,
@;   - was each step along the way performant?
