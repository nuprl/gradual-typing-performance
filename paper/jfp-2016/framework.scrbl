#lang scribble/base

@; TODO lattice
@;@figure["fig:fsm-lattice" "FSM performance lattice (labels are speedup/slowdown factors)"
@;  @(let ([vec (file->value "src/fsm-lattice-data.rktd")])
@;     (make-performance-lattice vec))
@;]

@require["common.rkt"]

@; -----------------------------------------------------------------------------
@title[#:tag "sec:frameworkfwk"]{Evaluation Framework}

@; @section{Criteria for Performance Evaluation}
Our experience with Typed Racket demonstrates two essential points:
  @itemlist[
    @item{
      Racket programmers are not seeking "ideal" conversion paths
       from untyped to typed.
      More often, they arrive at a completely random lattice point through
       some combination of existing code, necessity, and curiosity.
    }
    @item{
      Performance can fluctuate wildly depending on what subset of modules
       are typed due to the complex interaction between modules.
    }
  ]
These observations convince us that performance evaluation for gradual type
 systems needs to consider @emph{all possible} ways of gradually typing a
 program.

@; -----------------------------------------------------------------------------

@section{Performance Lattice}

Our evaluation framework is inspired by our previous work on extending functional
 Typed Racket to the object-oriented aspects of Racket, in which we
 use a lattice-style approach for a preliminary performance evaluation@~cite[tfdffthf-ecoop-2015].
By inspecting all possible ways of typing two small
 game systems, we identified and then eliminated a major
 performance bottleneck from the implementation.
Our previous performance evaluation was conducted in tandem with the
 design and implementation of Typed Racket, and thus the final results were
 relatively positive.
In contrast, we conduct our current evaluation completely @emph{independently} of
 Typed Racket's implementation efforts.@note{In terminology
  borrowed from the
  education community@~cite[scriven-chapter-1967], we conducted a @italic{formative
  evaluation} while this paper conducts a @italic{summative
  evaluation} to assess the post-intervention state of the system.}
Thus our framework is directly applicable to other sound gradual type systems.

Let us re-articulate the salient points from our previous work:
@itemlist[

@section{Performance Lattice}

The promise of macro-level gradual typing is that any subset of modules
 in an untyped program may be given types.
Performance evaluation must therefore consider the space of all @emph{configurations}
 of a program that can be obtained by adding types to a subset of the program's modules.
We describe this space as a static lattice representing all combinations of typed and
 untyped modules.
This lattice-based approach to performance measurements was introduced in our
 preliminary evaluation of Typed Racket's object-oriented
 features@~cite[tfdffthf-ecoop-2015]; here we summarize the key points:
@itemlist[
  @item{
    A (@emph{software system}) @emph{configuration} is a sequence of
     @math{N} modules where each module is either typed or untyped.
  }
  @item{ @emph{Proposition:}
    For a fixed sequence of @math{N} modules there are @math{2^N} possible
     configurations.
  }
  @item{
    Let @math{S} be the set of all configurations for @math{N} modules.
    For @exact|{$c \in S$}| and @exact|{$i \in [0, N)$}|,
     let @exact|{$c(i) = 1 \mbox{ iff the } i^{\emph{th}}$}| module in the sequence is typed
     and
     let @exact|{$c(i) = 0 \mbox{ iff the } i^{\emph{th}}$}| module in the sequence is untyped.
  }

  @item{
    Define @exact|{$\leq\,\subseteq S \times S$}| as:
     @exact|{$c_1 \leq c_2 \iff \forall i\,. (c_1(i) = 1) \Rightarrow c_2(i) = 1$}|
  }

  @item{ @emph{Proposition:}
    @exact|{$(S, \leq)$}| is a complete lattice.
    The fully untyped configuration is the
     bottom element and the fully-typed configuration is the top
  }

  @item{
    Define an indexed @emph{type conversion} relation @exact{$\rightarrow_k\,\subset\,\leq$}
     such that @exact{$c_1 \rightarrow_k c_2$} if and only if @exact{$c_1 \leq c_2$}
     and the sum @exact{$\,\Sigma_i\,\big[c_2(i) - c_1(i)\big]$} is less than or equal to @exact{$k$}.
    If @exact|{$c_1 \rightarrow_k c_2$}| we say that @exact{$c_2$} is reachable from
     @exact{$c_1$} in at most @exact{$k$} @emph{type conversion steps}.
  }
]

@item{Each module@note{For micro gradual typing, each variable is either typed or untyped}
  in a software system configuration is either typed or untyped.}

@item{For a fixed sequence of @math{n} modules there are @math{2^n} possible
 configurations.}

@item{
 Let @math{S} be the set of all configurations for a sequence of @math{n} modules.
 For @exact|{$c \in S$}| and @exact|{$i \in [0, n)$}|,
  let @exact|{$c(i) = 1 \mbox{ iff the } i^{\emph{th}}$}| module in the sequence is typed
  and
  let @exact|{$c(i) = 0 \mbox{ iff the } i^{\emph{th}}$}| module in the sequence is untyped.}

@item{Define @exact|{$\leq\,\subseteq S \times S$}| as:
 @exact|{$c_1 \leq c_2 \iff \forall i\,. c_1(i) = 1 \Rightarrow c_2(i) = 1$}|}

@item{@emph{Proposition:} @exact|{$(S, \leq)$}| is a complete lattice.
 Proof is trivial. We remark that the fully untyped configuration is the
 bottom element and the fully-typed configuration is the top}
]
 We speak of a @italic{performance lattice} to describe a pair @exact|{$(S, \leq)$}|.


Our contribution is to exploit the lattice-oriented approach to benchmarking
 for a @emph{summative} evaluation.
To this end, we imagine software engineers who are considering the use of
 gradual typing for some program and consider what kinds of questions may
 influence their decision.
Based on this first step, we formulate a small number of parameterized,
 quantitative measures that capture possible answers to these questions.
Our criteria for gradual type system design is thereby in sync with the
 performance properties most useful to developers.

@section{Measurements}

When the configuration consists of a small number of modules, the software engineers
 might be able to equip the entire program with type annotations in one fell
 swoop.
Such a fully annotated system should perform as well as
 the original, untyped version---and if the gradual type system is
 integrated with the compiler, it may even run faster because the compiler
 can apply standard type-based optimization techniques.
@; YUCK we just said developers will never reach the top.

@def[#:term "typed/untyped ratio"]{The typed/untyped ratio of a performance
 lattice is the time needed to run the top configuration divided by the
 time needed to run the bottom configuration.}

Unfortunately, this assumption overlooks the realities of implementations
 of gradual typing. Some modules simply cannot be equipped with types
 because they use linguistic constructs that the type system does not support.
 Furthermore, completely typed configurations still use the
 run-time libraries of the underlying untyped language. In particular,
 Typed Racket's run-time system remains largely untyped. As a result, even
 the completely typed configurations of our benchmarks usually import
 constants, functions, and classes from an untyped module in the run-time
 system. When these values cross this boundary at run-time, the contract
 system performs checks, and that imposes additional costs. To address this
 issue, the implementors of Typed Racket have enlarged their trusted code
 base with unchecked type environments that cover frequently imported parts
 of the run-time system. The next
 section explains what ``completely typed'' means for the individual
 benchmarks. 

When the software system configuration consists of a reasonably large
 number of modules, no software engineering team can annotate the entire
 system with types all at once. Every effort is bound to leave the configuration
 in a state in which some modules are typed and some others are untyped. As a
 result, the configuration is likely to suffer from the software contracts
 that the gradual type system injects at the boundaries between the typed and
 the untyped portions. If the cost is tolerable, the configuration can be
 released and can replace the currently deployed version. 
 The run-time costs may not be tolerable,
 however, as our previous work observes. In that case, the question is how
 much more effort the software engineers have to invest to reach a releasable
 configuration. That is, how many more modules must be converted before the
 performance is good enough for the new configuration to replace the
 running one.

To capture this idea, we formulate the following definition of
 ``deliverable configurations.''

@def[#:term @list{@deliverable{N}}]{A configuration in a performance
 lattice is @deliverable{N} if its performance is no worse than an @math{N}x slowdown
 compared to the completely untyped configuration.}

 We parameterize this definition over the slowdown factor that a team
 may consider acceptable. One team may think of a 1.1x slowdown as barely
 acceptable, while another one may tolerate a slowdown of an order of
 magnitude@~cite[tfdffthf-ecoop-2015].

Even if a configuration is not deliverable, it might be suitably fast to
 run the test suites and prototype designs.
A software engineering team can
 use such a configuration for development purposes, but it may not
 release it to clients.
@;In practice, usable configurations may act as checkpoints for a team to ensure
@; the product is working correctly before implementing performance optimizations.
Using a second parameter to capture the meaning of "suitably fast",
 we define a notion of usable configurations.

    @def[#:term @list{@usable[]}]{
     A configuration in a performance
      lattice is @usable[] if its performance is worse than an
      @math{D}x slowdown but no worse than a @math{U}x slowdown compared to
      the untyped configuration.
    }

On the other hand, the performance overhead of gradual typing may render
 some configurations too slow even for development purposes.
These might be configurations where running the unit tests takes hours
 or days longer than normal.

    @def[#:term "unacceptable"]{
     A unacceptable configuration is neither @deliverable{} nor @usable[].
    }

Finally, if a software project is currently in an unacceptable
 configuration we ask how much work a team needs to invest to reach a
 deliverable or usable configuration.
We propose as a coarse measure of "work" the number of modules that must be
 annotated with types before performance improves.
Using this metric, configurations one module away from a usable configuration
 are recognized as nearly usable themselves.

    @def[#:term @list{@step{}}]{
     A configuration is @step[] if it is unacceptable and at most @math{k}
      type conversion steps from a @deliverable{}
      or a @usable[] configuration.
    }

@profile-point{sec:framework:example}
@(define sample-data
  (let* ([mean+std* '#((2 . 0) (1.5 . 0) (3.5 . 0) (1 . 0))]
         [mean (lambda (i) (car (vector-ref mean+std* i)))])
    (lambda (tag)
      (case tag
       [(c00) (mean 0)]
       [(c01) (mean 1)]
       [(c10) (mean 2)]
       [(c11) (mean 3)]
       [(all) mean+std*]
       [else (raise-user-error 'sample-data "Invalid configuration '~a'. Use e.g. c00 for untyped." tag)]))))
@(define (sample-overhead cfg)
  (ceiling (/ (sample-data cfg) (sample-data 'c00))))

The four notions of the typed/untyped ratio, @deliverable{},
 @usable[], and @step[] form the basis of our evaluation
 framework.
As an example of these terms' use, suppose we have a project of
 two modules where the untyped configuration runs in @id[(sample-data 'c00)]
 seconds and the typed configuration runs in @id[(sample-data 'c11)] second.
Furthermore, suppose the gradually typed configurations run in
  @id[(sample-data 'c01)] and @id[(sample-data 'c10)] seconds.
Using white squares to represent untyped modules and black squares for typed
 modules, we can visualize this scenario in a performance lattice with
 configurations' overhead (relative to the untyped configuration) as labels.

@centered[@elem[
  (parameterize ([*LATTICE-HSPACE* 30]
                 [*LATTICE-VSPACE* 4])
    (make-performance-lattice (sample-data 'all)))
]]

@(let ([tu-ratio (/ (sample-data 'c11) (sample-data 'c00))]
       [t-str @id[(sample-overhead 'c11)]]
       [g-overhead (inexact->exact (max (sample-overhead 'c10) (sample-overhead 'c01)))])
  @elem{
    In terms of our metrics, typed/untyped ratio is @id[tu-ratio],
     indicating a performance improvement due to adding types.
    The typed configuration is also
      @deliverable[t-str]
      because it runs within a @elem[t-str]x
      slowdown relative to the untyped configuration.
    Both gradually typed configurations are
      @deliverable[@id[g-overhead]]
      because they run within @id[(* g-overhead (sample-data 'c00))] seconds.
    These configurations are also @step[t-str t-str t-str]
     because each is one conversion step
     from the fully-typed configuration.
  })

Practitioners curious about the feasibility of gradual typing should
 replace the parameters with concrete values tailored to their needs.
If our experimental results in @Secref{sec:plots} show that a large number of
 configurations are deliverable under the actual parameters,
 then the same results may hold for other projects.
Language implementors should work to make more configurations deliverable
 or diagnose the cause of the worst performance overhead.
