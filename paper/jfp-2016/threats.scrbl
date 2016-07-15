#lang scribble/base

@require["common.rkt" "fig-contract-profile.rkt"]

@title[#:tag "sec:threats"]{Threats to Validity of Conclusion}


The application of our evaluation method projects a very negative image of
 sound gradual typing.
While we are confident that the method captures the spirit of the goals of
 gradual typing, our particular application must be put in perspective.

First, our benchmarks are relatively small due to constraints on our
 computing infrastructure, but even those consume considerable resources.
Running benchmarks simultaneously on different cores of the same machine
 may introduce confounding variables due to, e.g., shared caches or main memory.
We have attempted to control for this case and, as far as we can tell,
 executing on an unloaded machine does not make a significant difference.
 @todo{confirm}

Second, several of our benchmarks are not self-contained, but import untrusted
 code from an external library.
These impose a cost on all configurations.
In principle, these interfaces might substantially contribute to the
 running-time overhead of partially typed configurations.
Regardless, given the low typed/untyped ratios, 
 these libraries are unlikely to affect our conclusions.
@; What is the point of this one?
@; If we haven't already said it, gradual typing is a way of life

Third, the feasible set of type annotations for a program component is
 rarely unique in a gradually typed system.
Since types are translated into contracts in Typed Racket, the choice of
 type annotations may affect performance.
@todo{quad, fsm}
Generally speaking, our results may not be fully representative.
 Then again, it is still a failure of gradual typing if a programmer must
 divine the best possible type annotations to obtain reasonable
 performance.

Finally, we articulate our conclusions on the basis of current
 implementation technology.
Typed Racket compiles to Racket, which uses
 rather conventional JIT compilation technology.
It makes no attempt to reduce the overhead of contracts or to exploit contracts
 for optimizations.
It remains to be seen whether contract-aware compilers can reduce the
 significant overhead that our evaluation shows.
Nevertheless, we are convinced that even if the magnitude of the slowdowns are
 reduced, some pathologies will remain.



