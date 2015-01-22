#lang scribble/base

@require["common.rkt"]

@title{The State of Gradual Typing Performance}

Gradually typed programming languages promise to simplify software maintenance
by enabling programmers to selectively add type annotations to an existing
untyped program. The promise also implies that as programmers add type annotations,
their program will continue to function correctly. To maintain that promise, gradual type
systems allow untyped and typed code to link together. At these linkage
points, gradual type systems insert dynamic check in the form of
@emph{casts} or @emph{contracts} to ensure sound interoperation.

For successful software maintenance, programmers may also require that the
program remains @emph{performant}. In this regard, existing gradual type systems
may fail to live up to their promises. Gradual type systems in the literature
report slowdowns of 72x@~cite[rsfbv-popl-2015], 10x@~cite[vksb-dls-2014],
and 4x@~cite[tfdffthf-ecoop-2015] in programs due to the insertion of
dynamic checks.

