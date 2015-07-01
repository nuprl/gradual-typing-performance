#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:intro"]{Gradual Typing and Performance}

Over the past couple of decades dynamically-typed languages have been a
 staple of the software engineering world. Programmers use these languages
 to build all kinds of software systems. In many cases, the systems start
 as innocent prototypes. Soon enough, though, they grow into complex,
 multi-module programs, at which point the engineers realize that they are
 facing a maintenance nightmare, mostly due to the lack of reliable type
 information. 

Gradual typing@~cite[st-sfp-2006 thf-dls-2006] proposes a language-based
 solution to this pressing software engineering problem. The idea is to
 extend the language so that programmers can incrementally equip programs
 with types. In contrast to optional type systems@~cite[bg-oopsla-1993],
 gradual type systems provide programmers with soundness guarantees, that
 is, the type annotations should be meaningful and predict run-time
 behavior. 

Realizing type soundness in this world requires run-time checks that watch
 out for potential impedance mismatches between the typed and untyped
 portions of the programs. The granularity of these checks will determine
 the peformance overhead of gradual typing. To reduce the frequency of
 checks, macro-level gradual typing forces programmers to annotate entire
 modules with types and relies on behavioral contracts@~cite[ff-icfp-2002]
 between typed and untyped modules enforce soundness. Micro-level gradual
 typing assigns an implicit type @tt{Dyn}@~cite[TypeDynamic] to all
 unannotated parts of a program, type annotations can then be added to any
 declaration. It is up to the implementation to insert casts in the
 appropriate points in the code; different language design have slightly
 different semantics with different associated costs and limitations.

Both approaches to gradual typing come with two implicit claims. First, the
 type systems accommodate common untyped programming idioms.  This is to
 allow programmers to add types with minimal changes to existing
 code. Second, the cost of soundness is tolerable, meaning programs remain
 performant even as programmers add type annotations. Ideally, types should
 improve performance as they provide invariants that an optimizing compiler
 can leverage to generate better code.  While almost every publication on
@; The whole point of the Thorn/StrongScript design is to tackle that second
@; claim. This done by adding restricitions (e.g. nominal...) but we definitely
@; have no allocation of wrappers.  Tests boil down to constant time checks.
 gradual typing validates some version of the first claim, few projects
 tackled the second claim. Most publications have qualified remarks about the
 performance of partially typed programs; some plainly admit that such mixed
 programs may suffer performance degradations of two orders of magnitude.

This paper introduces a methodology for the systematic performance
 evaluation of gradual typing systems. The paper's insight is that to
 understand the performance of a gradual type system it is necessary to
 study how a maintenance programmer may choose to add types to an existing
 software system. For practical reasons, such as time or access to source
 code, it may only be possible to add types to a part of the system.  In the
 context of a macro-level gradual type system, the basic idea is to simulate
 the software engineering process on multi-module programs. All @exact{$n$}
 modules are annotated with types, and the resulting collection of @exact{$2
 \cdot n$} modules is then used to create all @exact{$2^n$}
 configurations. The collection of these configurations forms a complete
 lattice with the untyped system at the bottom and the completely typed one
 at the top. In between, the lattice contains configurations where some
 modules are typed and others are untyped. Adding types to an untyped module
 in one of these configurations yields a configuration at the next level in
 the lattice. In short, the lattice mimics all possibilities of how a
 programmer may pick one module out of many and add types when a maintenance
 task comes up.

A performance measurement of a gradual typing system must run every
 configuration for every benchmark and extract information from the
 resulting lattices of performance measurements.  The latter may ask such
 basic questions as how many of these configurations could easily be
 deployed without affecting performance too much.

We validate our methodology by evaluating Typed Racket, the gradually typed
 sister language of Racket, on a collection of programs ranging from 150 to
 15K lines of code.  Typed Racket is the oldest (developed since 2006) and
 probably most sophisticated implementation of gradual typing, but it is
 also a natural choice because its macro-level approach to gradual typing
 appears to impose the lowest cost for boundaries between typed and untyped
 modules. Furthermore, since Racket is a widely used programming language,
 Typed Racket has also rapidly acquired a fair number of users in the
 commercial and open source community, which suggests at least adequate
 performance.@note{Personal communication with the implementors, who claim
 some 100,000 unique downloads per year.}

Section@secref{sec:fwk} explains the performance framework in detail,
 including the information we retrieve from the lattices and how we
 parametrize these retrievals. Next, section@secref{sec:bm} presents our
 specific Typed Racket benchmarks. Section@secref{sec:tr} @; and@secref{sec:trp}
 presents the numeric results.
@;  of evaluating Typed Racket's original
@;  implementation and the Pycket variant,
@; respectively. 
 Section@secref{sec:death} discusses these
 results. Section@secref{sec:rel} reviews the literature on gradual typing
 systems with respect to performance evaluation. Section@secref{sec:fut}
 concludes with ideas for future work.
