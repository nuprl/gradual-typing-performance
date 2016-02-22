#lang scribble/base

@; TODO lattice
@;@figure["fig:fsm-lattice" "FSM performance lattice (labels are speedup/slowdown factors)"
@;  @(let ([vec (file->value "src/fsm-lattice-data.rktd")])
@;     (make-performance-lattice vec))
@;]

@require["common.rkt"]

@; -----------------------------------------------------------------------------
@title[#:tag "sec:framework"]{Evaluation Framework}

@; @section{Criteria for Performance Evaluation}
Our experience with Typed Racket demonstrates two essential points:
  @itemlist[
    @item{
      Programmers are not seeking "ideal" conversion paths
       from untyped to typed.
      More often, they create a mixed typed/untyped program through
       some combination of existing code, necessity, and convenience.
    }
    @item{
      Performance can fluctuate wildly depending on the number of typed modules
       and the nature of their interactions.
      These fluctuations are difficult to predict
       even for the program's original author.
      @todo{is this claim supported by the text?}
    }
  ]
These observations convince us that performance evaluation for gradual type
 systems needs to consider @emph{all possible} ways of gradually typing a
 program.
We use take these observations as guiding principles for a @emph{language agnostic}
 evaluation framework.

@; -----------------------------------------------------------------------------

@section{Performance Lattice}

Our evaluation framework is inspired by our previous work on extending functional
 Typed Racket to the object-oriented aspects of Racket, in which we
 use a lattice-style approach for a preliminary performance evaluation
 conducted in tandem with the design and implementation@~cite[tfdffthf-ecoop-2015].
By inspecting all possible ways of typing two small
 game systems, we identified and then eliminated a major
 performance bottleneck from the implementation.
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

@item{Each module in a software system configuration is either typed or untyped.}

@item{For a fixed sequence of @math{n} modules there are @math{2^n} possible
 configurations.}

@item{
 Let @math{S} be the set of all configurations for a sequence of @math{n} modules.
 For @exact|{$c \in S$}| and @exact|{$i \in [0, n)$}|,
  let @exact|{$c(i) = 1 \mbox{ iff the } i^{\emph{th}}$}| module in the sequence is typed
  and
  let @exact|{$c(i) = 0 \mbox{ iff the } i^{\emph{th}}$}| module in the sequence is untyped.}

@item{Define @exact|{$\leq\,\subseteq S \times S$}| as:
 @exact|{$c_1 \leq c_2 \iff \forall i\,. (c_1(i) = 1) \Rightarrow c_2(i) = 1$}|}

@item{@emph{Proposition:} @exact|{$(S, \leq)$}| is a complete lattice.
 The fully untyped configuration is the
 bottom element and the fully-typed configuration is the top}
]
We speak of a @italic{performance lattice} to describe a pair @exact|{$(S, \leq)$}|
 generated from a sequence of modules.
Note that a performance lattice does not actually contain performance numbers.
It is a static artifact representing the possible ways of running the program.
Actually testing the configurations and recording the time taken is next.

Our contribution is to exploit the lattice-oriented approach to benchmarking
 for a @emph{summative} evaluation.
To this end, we imagine software engineers who are considering the use of
 gradual typing for some program and consider what kinds of questions may
 influence their decision.
Based on this first step, we formulate a small number of parameterized,
 quantitative measures that capture possible answers to these questions.
The goal is that we as language designers evaluate performance using
 criteria that our users find valuable.


@section[#:tag "sec:measurements"]{Measurements}

When the software system consists of a small number of modules, the engineers
 might be able to equip the entire program with type annotations in one fell
 swoop.
Such a fully annotated system should perform as well as
 the original, untyped program---and if the gradual type system is
 integrated with the compiler, it may even run faster because the compiler
 can apply type-based optimization techniques.
Unfortunately, completely typed configurations may still rely on untyped
 libraries or core services.
In particular, Typed Racket relies on Racket's untyped runtime system, hence
 programs almost always use an untyped constant, function, or class across
 a type boundary.@note{To mitigate the cost of this boundary,
  part of Racket's runtime is put in an unchecked type environment.}

@def[#:term "typed/untyped ratio"]{The typed/untyped ratio of a performance
 lattice is the time needed to run the top configuration divided by the
 time needed to run the bottom configuration.}

Sufficiently large systems cannot be fully converted all at
 once and may never even reach the top configuration.
In theory this is not a problem, as gradual typing promises to safely run
 any combination of typed and untyped modules.
But the cost of preserving type soundness may make some combinations impractical
 to run.

If the performance overhead is low, the configuration can be
 released and replace the currently deployed program.
To capture this idea, we formulate the following definition of
 ``deliverable configurations.''

@def[#:term @list{@deliverable{N}}]{A configuration in a performance
 lattice is @deliverable{N} if its performance is no worse than an
 @math{N}x slowdown compared to the completely untyped configuration.}

We parameterize this definition over the slowdown factor that a team
 may consider acceptable.
One team may think of a 1.1x slowdown as barely
 acceptable, while another one may tolerate a slowdown of an order of
 magnitude.

Even if a configuration is not deliverable, it might be suitably fast to
 run the test suites and prototype designs.
A software engineering team can
 use such a configuration for development purposes, but it may not
<<<<<<< HEAD
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
=======
 deliver it.
In order to formulate this criteria
 properly, we introduce the following definition of usable configurations. 

@def[#:term @list{@usable["N" "M"]}]{A configuration in a performance
 lattice is @usable["N" "M"] if its performance is worse than an
 @math{N}x slowdown but no worse than an @math{M}x slowdown compared to
 the completely untyped configuration.}

Using the first parameter, we exclude deliverable configurations from the count.
The second parameter specifies the positive boundary, i.e., the acceptable
 slowdown factor for a usable configuration.
Conversely:
@; TODO define 'acceptable', inline 'unacceptable'?

@def[#:term "unacceptable"]{A
 configuration is unacceptable if it is neither @deliverable{N} nor
 @usable["N" "M"].}

Finally, we can also ask the question how much work a team has to invest to
 turn unacceptable configurations into useful or even deliverable
 configurations.
In the context of macro-level gradual typing, one easy way
 to measure this amount of work is to count the number of modules that have
 to be annotated with types before the resulting configuration becomes
 usable or deliverable.

@def[#:term @list{@step["L" "N" "M"]}]{A configuration is
 @step["L" "N" "M"] if it is unacceptable and at most @math{L} type
 conversion steps away from a @deliverable{N} or a @usable["N" "M"]
 configuration.}

These four notions of the typed/untyped ration, @deliverable{N},
 @usable["N" "M"], and @step["L" "N" "M"] form the basis of our evaluation
 framework.
The benefit of the parameterized metrics is that
 every reader can interpret the raw data with his or her own choices for
 @math{L}, @math{N}, and @math{M}.


>>>>>>> 698f2e7... [jfp] story, first pass
