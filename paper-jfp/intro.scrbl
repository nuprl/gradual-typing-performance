#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:intro"]{Gradual Typing and Performance}

Over the past couple of decades dynamically-typed languages have become a
 staple of the software engineering world. Programmers use these languages
 to build all kinds of software systems. In many cases, the systems start
 as innocent prototypes. Soon enough, though, they grow into complex,
 multi-module programs, at which point the engineers realize that they are
 facing a maintenance nightmare, mostly due to the lack of reliable type
 information. 

Gradual typing@~cite[st-sfp-2006 thf-dls-2006] proposes a language-based
 solution to this pressing software engineering problem. The idea is to
 extend the language so that programmers can incrementally equip programs
 with types. In contrast to optional typing,
 gradual typing provide programmers with soundness guarantees.

Realizing type soundness in this world requires run-time checks that watch
 out for potential impedance mismatches between the typed and untyped
 portions of the programs. The granularity of these checks determine
 the peformance overhead of gradual typing. To reduce the frequency of
 checks, @emph{macro-level} gradual typing forces programmers to annotate entire
 modules with types and relies on behavioral contracts@~cite[ff-icfp-2002]
 between typed and untyped modules to enforce soundness.
 In contrast, @emph{micro-level} gradual
 typing instead assigns an implicit type @tt{Dyn}@~cite[TypeDynamic] to all
 unannotated parts of a program; type annotations can then be added to any
 declaration. The implementation must insert casts at the
 appropriate points in the code. Different language designs use slightly
 different semantics with different associated costs and limitations.

Both approaches to gradual typing come with two implicit claims. First, the
 type systems accommodate common untyped programming idioms.  This allows
 programmers to add types with minimal changes to existing
 code. Second, the cost of soundness is tolerable, meaning programs remain
 performant even as programmers add type annotations. Ideally, types should
 improve performance as they provide invariants that an optimizing compiler
 can leverage. While almost every publication on
@; The whole point of the Thorn/StrongScript design is to tackle that second
@; claim. This done by adding restricitions (e.g. nominal...) but we definitely
@; have no allocation of wrappers.  Tests boil down to constant time checks.
 gradual typing validates some version of the first claim, no projects
 tackle the second claim systematically. Most publications come with qualified remarks about the
 performance of partially typed programs. Some plainly admit that such mixed
 programs may suffer performance degradations of up to two orders of magnitude
 @~cite[vksb-dls-2014 rsfbv-popl-2015 tfdffthf-ecoop-2015].

This paper presents a single result: a method for systematically evaluating the
performance of a gradual type system. It is illustrated with an application
to Typed Racket, a mature implementation of
macro-level gradual typing. We find that Typed Racket's cost of soundness is @emph{not}
tolerable. If applying our method to other gradual
type system implementations yields similar results, then sound gradual typing is dead.

The insight behind the method is that to
 understand the performance of a gradual type system, it is necessary to
 simulate how a maintenance programmer chooses to add types to an existing
 software system. For practical reasons, such as limited developer resources or access to source
 code, it may be possible to add types to only a part of the system.
 Our method must therefore simulate all possibilities.
 Thus, applying our method to Typed Racket requires annotating
 all @exact{$n$} modules with types. The resulting collection of @exact{$2
 \cdot n$} modules is then used to create @exact{$2^n$}
 configurations. The collection of these configurations forms a complete
 lattice with the untyped configuration at the bottom and the fully typed one
 at the top. The points in between represent configurations in which some
 modules are typed and others are untyped. Adding types to an untyped module
 in one of these configurations yields a configuration at the next level of
 the lattice. In short, the lattice mimics all possible choices of single-module
 type conversions a programmer faces when a maintenance task comes up.

A performance evaluation of a system for gradual typing must time these
 configurations of a benchmark and extract information from these
 timings. Section@secref{sec:fwk} introduces the evaluation method in
 detail, including the information we retrieve from the lattices and how we
 parameterize these retrievals.  The timings may answer basic questions
 such as how many of these configurations could be deployed without
 degrading performance too much.

We apply our method to Typed Racket, the gradually typed sister language of
 Racket.  With nine years of development, Typed Racket is the oldest and
 probably most sophisticated implementation of gradual typing.
 Furthermore, Typed Racket has also acquired a fair number of users, which
 suggests adequate performance for these commercial and open source
 communities. The chosen benchmark programs originate from these
 communities and range from 150 to 7,500 lines of code. @Secref{sec:bm}
 presents these benchmarks in detail.

Section@secref{sec:tr} presents the results from running all configurations
 of the Typed Racket benchmarks according to the metrics spelled out in
 section@secref{sec:fwk}.  We interpret the ramifications of these rather
 negative results in section@secref{sec:death} and discuss the threats to
 validity of these conclusions. The section also includes our report on a
 preliminary investigation into the possible causes of the slowdowns in our
 benchmark configurations.
