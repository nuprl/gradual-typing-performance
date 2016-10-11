#lang scribble/base
@require["common.rkt" (only-in racket/list add-between)]
@;@exact{\newpage}

@; TODO
@; - [ ] full module graphs for all benchmarks
@;       with colors + static + dynamic annotations
@; - [ ] mapping from 0-15 to configurations

@title[#:tag "appendix"]{Appendix}
@section{Bibliography of Performance Costs}

@(define month*
   '( "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))

@(struct abib [title author url date desc])

@(define (abib<? ab1 ab2)
   (string<? (abib-date ab1)
             (abib-date ab2)))

@(define (abib->elem ab)
   @list[@elem{@noindent[]@emph[(abib-title ab)].@exact{~~}@abib-author[ab].@exact{~~}@abib-date[ab].

               @noindent[1.2]@smaller[@url[(abib-url ab)]]}
         @exact|{\vspace{-1ex}}|
         @inset[@abib-desc[ab]]])

@(define (render-annotated-bib ab*)
   (cons (parag)
         (add-between (map abib->elem (sort ab* abib<?))
                      (parag))))

@(define (annotated-bib #:title t #:author a #:url u #:date d . descr)
   (abib t a u d descr))

@; ---

The following resources describe performance costs due to typed/untyped interaction.

@render-annotated-bib[@list[
  @annotated-bib[#:title "warning on use trie functions in #lang racket?"
                 #:author "John B. Clements"
                 #:url "https://groups.google.com/d/msg/racket-users/WBPCsdae5fs/J7CIOeV-CQAJ"
                 #:date "2016-01-05"]{
    Provides a script that runs twelve seconds slower when untyped.
    This script interacts with a typed trie library.
  }
  @annotated-bib[#:title "Generative Art in Racket"
                 #:author "Rodrigo Setti"
                 #:url "http://con.racket-lang.org/2016/setti.pdf"
                 #:date "2016-09-18"]{
    States that the cost of interaction between untyped code and the @library{math/array} library needs improvement.
  }
  @annotated-bib[#:title "Typed/Untyped cost reduction and experience"
                 #:author "JCG"
                 #:url "https://groups.google.com/d/msg/racket-users/rfM6koVbOS8/klVzjKJ9BgAJ"
                 #:date "2015-12-26"]{
    Reports a 50% performance overhead in an application using a 1,900-line Typed Racket server process.
  }
  @annotated-bib[#:title "Rocking with Racket"
                 #:author "Marc Burns"
                 #:url "http://con.racket-lang.org/2015/burns.pdf"
                 #:date "2015-09-27"]{
    Reports one startup's experience converting a database API to Typed Racket.
    After converting, the new code is less prone to bugs and more maintainable, but runs @exact{``}about twice as slow on common queries."
  }
  @annotated-bib[#:title "re: Unsafe version of require/typed?"
                 #:author "Neil Toronto"
                 #:url "https://groups.google.com/d/msg/racket-users/oo_FQqGVdcI/p4-bqol5hV4J"
                 #:date "2015-05-01"]{
    Reports performance issues that arose in the development of the typed @library{math}, @library{plot}, and @library{pict3d} libraries.
    Sending picture objects across a type boundary made some programs unresponsive and led to large memory overhead (7-14MB) in others.
    An FFI type boundary to OpenGL decreased the number of calls per 60Hz frame from 60,000 to 5,000.
    Polymorphic matrix operations ran over 50x slower when called from untyped code.
  }
  @annotated-bib[#:title "re: Unsafe version of require/typed?"
                 #:author "Michael Ballantyne"
                 #:url "https://groups.google.com/d/msg/racket-users/oo_FQqGVdcI/leUnIAn7yqwJ"
                 #:date "2015-05-01"]{
    Provides a script that runs 14 seconds slower when untyped.
    The script interacts with a typed queue library.
  }
]]


@section{The @|GTP| Benchmark Programs, by Module}

The following summaries describe the module-level structure of programs in the @|GTP| benchmark suite.
In particular, the summaries include:
@itemlist[
  @item{
    the name and purpose of each module;
  }
  @item{
    a table of each modules' static characteristics (lines of code, number of exports);
  }
  @item{
    a graph of inter-module dependencies, annotated with the number of times
    values crossed each module boundary during our experiment.
  }
]

@; The module names for each program are listed alphabetically.
@; suffixtree lattice, morsecode configs

@; -----------------------------------------------------------------------------
@;
@; @(require
@;   (except-in gtp-summarize/lnm-parameters defparam)
@;   (only-in gtp-summarize/lnm-plot plot-indirection-cost))
@;
@;    @figure["fig:adaptor" "Indirection cost of adaptors"
@;      @(parameterize([*CONFIDENCE-LEVEL* 98]
@;                     [*PLOT-WIDTH* 460]
@;                     [*PLOT-HEIGHT* 280])
@;         (plot-indirection-cost "src/adaptor-vs-no-adaptor.rktd"))
@;    ]

@; MODULEGRAPHS
@; and use a graph structure to represent the interactions
@;  of its modules.
@; Nodes in the graphs represent modules in the program that our experiment
@;  varies as typed or untyped.
@; Edges represent static import statements.
@; For example, the leftmost node in each graph represents the program's main module,
@;  which imports from other modules but is never itself imported.
@; Finally, we color and thicken each edge in proportion to the runtime cost
@;  associated with the edge.
@; @todo{what are colors/what mean?}
@; 
