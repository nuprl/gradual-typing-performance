#lang scribble/base

@require["common.rkt"]

@require[pict
         racket/file
         racket/vector
         math/statistics
         (only-in racket/match match-define)
         "render-lnm.rkt"
         "../tools/data-lattice.rkt"]

@title[#:tag "sec:tr"]{Evaluating Typed Racket, Classical}

Typed Racket as evaluated at NU 

@section{A Benchmark in Depth}

In order to explain our experimental setup, we take a closer look at the
@tt{suffixtree} benchmark and explain the pieces that are involved.

The benchmark consists of six main modules which are each available with
and without type annotations:
@tt{data.rkt}, @tt{label.rkt}, @tt{lcs.rkt}, @tt{main.rkt}, @tt{structs.rkt},
and @tt{ukkonen.rkt}.

Typed Racket distinguishes importing a value from a typed module and from an
untyped module. When importing from an untyped source, Typed Racket requires
that the user write down a type annotations using the @racket[require/typed]
form. This form explicitly adds a dynamic check to ensure that the imported
value truly satisfies that type. When the type is higher-order (e.g.,
function or class types), the dynamic check is delayed by wrapping the
imported value with a contract wrapper.

Since modules in our benchmark setup may be either typed or untyped depending
on the configuration, we modify all @racket[require/typed] imports to use a
@racket[require/typed/check] form that installs the dynamic check if the source
module is untyped and ignores the annotation if the source module is typed.
This allows typed modules in the benchmark to work independently of the
configuration of other modules.

For example, the original untyped @tt{lcs.rkt} module contains the import
statement @racket[(require "label.rkt")]. In the ordinary typed version,
this is rewritten to
@;
@racketblock[(require/typed "label.rkt"
                            [label->string (-> Label String)])]
@;
with a type annotation for each
imported value. Finally, in the benchmark instrumented version we replace
@racket[require/typed] with @racket[require/typed/check].

@subsection{Performance of Suffixtree}

@figure*["fig:suffixtree" "Suffixtree performance lattice. Nodes labeled with normalized mean (top) and standard deviation (bottom)."
  @(let* ([vec (file->value "../tools/data/suffixtree-2015-04-02.rktd")]
          [vec* (vector-map (λ (p) (cons (mean p) (stddev p))) vec)])
     (make-performance-lattice vec*))
]

To further understand the @tt{suffixtree} benchmark, we will look at its
performance lattice, shown in @figure-ref{fig:suffixtree}. The lattice
in the figure displays each of the modules in the program with a circle.
A filled black circle means the module is typed, an open circle means the
module is untyped. The circles are ordered from left to right and correspond
to the modules of @tt{suffixtree} in alphabetical order: 
@tt{data.rkt}, @tt{label.rkt}, @tt{lcs.rkt}, @tt{main.rkt}, @tt{structs.rkt},
and @tt{ukkonen.rkt}. Each configuration lists the average (on top) and standard deviation
(below) of the runtime of 30 iterations normalized to the untyped average.

Following the definitions in @secref{sec:fwk}, we can first determine the
typed/untyped ratio. The fully typed configuration (the top one)
for @tt{suffixtree} has the good property that
it is @emph{faster} than the fully untyped (bottom)
configuration by about 30%. This puts the ratio at about 0.7.

The typed configuration is faster due to Typed Racket's optimizer, which is
able to perform type-based specialization of arithmetic operations,
optimization of record field access, and elimination of some bounds
checking for vectors in @tt{suffixtree}. We determined that the optimizer
is responsible for the speedup by comparing the runtime of the fully typed
configuration with and without type-based optimization enabled.

Despite the speedup on the fully typed configuration, in-between configurations
have slowdowns that vary drastically from 1.02x to 100x. Inspecting
the lattice, several conclusions can be drawn about adding types to this
program. For example, adding type annotations to the @tt{main.rkt} module neither
subtracts or adds much overhead since it is a driver module that is not tightly
coupled to other modules. Comparatively, adding types to any of
@tt{data.rkt}, @tt{label.rkt}, or @tt{structs.rkt} from the fully
untyped configuration adds at least a 35x slowdown. This suggests that these
modules are highly coupled.

Inspecting @tt{data.rkt} and @tt{label.rkt} reveals, for example, that the
latter depends on the former through an adaptor module. The adaptor introduces
contract overheads when either of the two modules is untyped. When both
modules are typed but all other remain untyped, the slowdown is reduced to
about 12x.

The @tt{structs.rkt} module depends on @tt{data.rkt} in the same fashion.
However, since @tt{structs.rkt} also depends on @tt{label.rkt}, the configuration
in which both @tt{structs.rkt} and @tt{data.rkt} are typed still has a large
slowdown. When all three modules are typed, the slowdown is reduced to about
5x.

Another fact that the lattice shows is that the configurations whose
slowdown is closest to the worst case are those in which the @tt{data.rkt} module
is left untyped but several of the other modules are typed. This makes sense given
the coupling we observed above; the contract boundaries induced between the
untyped @tt{data.rkt} and other typed modules slow down the program.
The module structure diagram for @tt{suffixtree} in @figure-ref{fig:bm}
corroborates the presence of this coupling. The rightmost node in that
diagram corresponds to the @tt{data.rkt} module, which has the most in-edges in
that particular graph.


@section{Experimental Results}
@(Figure-ref "fig:lnm1" "fig:lnm2") summarize the results of creating a performance lattice for each benchmark program.
Rather than displaying the entire lattice for each of the 12 programs, we summarize the @emph{L-N/M} characteristics of each program with a row of figures.

@figure*["fig:lnm1" @list{@emph{L-step N/M-usable} results for selected benchmarks}
  @(let* ([rktd* '(
                   "./data/funkytown.rktd"
                   "./data/gregor-05-11.rktd"
                   "./data/kcfa-small-06-20.rktd" ;; TODO needs re-run, row 111 of data is malformed
                   "./data/quad-placeholder.rktd"
                   "./data/snake-04-10.rktd"
                   "./data/suffixtree-06-10.rktd"
                  )])
     (rktd*->pict rktd* #:tag "1"))
]

@figure*["fig:lnm2" @list{@emph{L-step N/M-usable} results for the rest of the benchmarks}
  @(let* ([rktd* '(
                   "./data/echo.rktd"
                   "./data/mbta-04-20.rktd"
                   "./data/morsecode-06-19.rktd"
                   ;"./data/sieve-04-06.rktd"
                   "./data/lnm-06-22.rktd"
                   "./data/tetris-large-06-20.rktd"
                   "./data/zordoz-04-09.rktd"
                  )])
     (rktd*->pict rktd* #:tag "2"))
]

@subsection{Reading the Figures}
@; Describe the lines & units on the figures

The line graphs show the number of variations that are @emph{L-step} usable for a particular @emph{L} and overhead factor.
Each line is the result of sampling @id[PARAM-NUM-SAMPLES] values linearly spaced along the x-axis.

Overhead factors range from 1x, indicating a speedup relative to the untyped program, to a generous @id[PARAM-MAX-OVERHEAD]x slowdown compared to the untyped variation.
@; TODO color "green" and "yellow"
@; TODO figure out the scribble @- to insert Racket parameters
To put these slowdown factors in perspective, we draw a @todo{green} vertical line at @id[PARAM-N]x overhead and a @todo{yellow} vertical line at @id[PARAM-M]x as suggested cutoffs for @emph{N} and @emph{M}.
That is, we consider anything to the left of the line at @id[PARAM-N]x to be usable and anything between that green line and the yellow line at @id[PARAM-M]x to be usable.

On each y-axis, we count the absolute number of variations in the program.
The y-axis labels range from 0 variations to @exact{$2^n$} variations, where @exact{$n$} is the number of modules in that row's benchmark program.
@; TODO color the word "red"
The axes themselves are scaled to be the same height for all figures; in particular, we draw a @todo{red} dashed line at the number corresponding to 60% of all variations in the program.

Each column of figures shows results for a fixed value of @emph{L}, ranging between 0 and @id[PARAM-L], inclusive.
Thus the leftmost column simply counts the number of variations with performance below a given overhead factor.
In contrast, the graphs in the rightmost column count all variations that are at most @id[PARAM-L] type-annotation steps away from a usable variation.

Lastly, each row of figures is accompanied by a brief summary of the performance statistics we measured.
These statistics include the number of modules in the program, the overhead of the fully-typed variation (@exact{$\tau$}), and the overhead of the worst-case and average-case gradually typed variations.
Note that the worst and average case numbers do not include the typed and untyped variation.


@subsection{Interpreting the Figures}
@; Judgments to make from the figures

The graphs help answer three high-level performance questions:
@itemlist[
@item{How many variations are deliverable for a fixed overhead @exact{$N$}?}
@item{How many variations become usable for an @exact{$M > N$}?}
@item{How does increasing the unit of work from 1 conversion step to 2 or 3 change the performance picture?}
]
The red dashed line represents our bottom line:
That is, supposing a developer has decided on fixed bounds for @exact{$N$}, @exact{$M$}, and @exact{$L$}, are at least 60% of the variations deliverable or at least usable?


We did not expect these graphs to be interesting---they should all look like @bold{gregor} or @bold{echo}.


@subsection{All Benchmarks, in some depth}
@; Due dilligence for each benchmark,
@; TODO we should re-title and compress this section before submitting


