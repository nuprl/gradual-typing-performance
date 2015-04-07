#lang scribble/sigplan @nocopyright

@(require "common.rkt"
          racket/vector
          math/statistics)

@authorinfo["Asumu Takikawa" "Northeastern University" "asumu@ccs.neu.edu"]
@authorinfo["Daniel Feltey" "Northeastern University" "dfeltey@ccs.neu.edu"]
@authorinfo["Ben Greenman" "Northeastern University" "types@ccs.neu.edu"]
@authorinfo["Max S. New" "Northeastern University" "maxsnew@ccs.neu.edu"]
@authorinfo["Jan Vitek" "Northeastern University" "j.vitek@ccs.neu.edu"]
@authorinfo["Matthias Felleisen" "Northeastern University" "matthias@ccs.neu.edu"]

@title{Position Paper: Performance Evaluation for Gradual Typing}

@section{The State of Gradual Typing Performance}

Gradually typed programming languages promise to improve software maintenance
by letting programmers selectively add type annotations to their existing
untyped programs. The promise implies that as programmers add type annotations,
their program will continue to function. To maintain that promise, gradual type
systems allow untyped and typed code to link together. At these boundaries,
gradual type systems insert dynamic checks in the form of
@emph{casts} or @emph{contracts} to ensure sound interoperation.

For successful software maintenance, programmers also wish that their
programs remain @emph{performant}. Therefore, a gradual type system ought to
also promise low overhead for interoperation.
In our experience, existing gradual type systems
(including the systems we maintain) fail to meet this criterion.
Several of the authors who are Typed Racket developers have received bug reports from users who observed
drastic slowdowns after adding types.
Gradual type systems in the literature report slowdowns of 72x@~cite[rsfbv-popl-2015],
10x@~cite[vksb-dls-2014], and 4x@~cite[tfdffthf-ecoop-2015] in programs due to the insertion of
dynamic checks.

To make gradual type systems live up to their promises, we must
(1) diagnose what kinds of programs and what degree of ``typedness'' leads
to performance problems, and (2) identify the tools, language features,
or implementation techniques that will help eliminate the overhead.
For now, we will focus on the diagnostic side and hope to investigate
solutions in the future.

@section{Exploring the Program Space}

We propose an evaluation of the current state of gradual type system overhead
by simulating the process a programmer would go through in order to add types
to a program. As a first attempt, several of the authors worked on a small-scale
version of this methodology in @citet[tfdffthf-ecoop-2015] in the context of
Typed Racket. We hope to use this methodology to diagnose the kinds of
performance issues that come up in gradually-typed programs.

Typed Racket is a @emph{macro}-level gradual type system, which means
that types are added to the program at whole-module granularity and dynamic checks
are installed at these boundaries between typed and untyped modules.
Since each module in a Typed Racket program
is either typed or untyped, a programmer can choose to add types to the program
in 2@superscript{n} possible configurations. We can represent this space of
configurations as a lattice in which the nodes represent a particular configuration
of modules in a program---that is, whether each module is typed or untyped.
An edge between two nodes A and B indicates that configuration A can be turned
into configuration B by adding type annotations to an additional module.
See @figure-ref{lattice-example} for an example of a program lattice. The
bottom of the lattice represents the original, fully untyped program and the top of
the lattice represents the program with types added to all modules.

@figure["lattice-example" "Lattice example with five modules"]{
  @(let* ([vec (file->value "zordoz-all-runs.rktd")]
          [vec* (vector-map (λ (p) (cons (mean p) (stddev p))) vec)])
     (scale (make-performance-lattice vec*) 0.7))
}

Paths in the graph that start from the bottom correspond to the timeline
of a hypothetical programmer who is adding types to the program.
Ideally, every configuration does not introduce unreasonable dynamic overhead.
In practice, however, portions of the lattice will contain regions of poor
performance due to, for example,
tightly coupled modules with dynamic checks on the boundary.

Continuing from @citet[tfdffthf-ecoop-2015], we are working on scaling this
evaluation methodology to programs with a larger number of modules
(and hence a much larger number of variations) and are
investigating both functional and object-oriented programs. The large
number of variations---2@superscript{n} for n modules---in particular makes
data visualization and analysis more difficult. We are therefore considering
alternatives to the lattice form of visualization such as histograms over
path metrics and heatmaps.

@section{Generalizing the Methodology}

While our methodology is straightforward for the macro style of gradual typing
used in Typed Racket, it is not obvious how to apply it to the @emph{micro}-level
gradual typing used by most other gradual type systems. By micro-level we mean
a gradual type system which allows typed and untyped code to mix freely at a
finer granularity than modules, typically using @tt{Dyn} types. Research systems
such as Gradualtalk, Reticulated Python, and Safe TypeScript all use the micro
approach. The micro approach, however, poses more of a challenge for our
methodology because it is not clear how to set up the space of variations.
For example, type annotations could be toggled by function, by module, or even
individually. Picking the latter would lead to a huge configuration space, however.

@section{Investigating Potential Solutions}

After diagnosing the kinds of overhead found in gradually-typed programs, we intend
to investigate possible solutions. Solutions may come in the form of mitigation,
in which a tool or language feature helps avoid problematic dynamic checks. Alternatively,
the solutions may instead seek to reduce the cost of the checks.

One form of mitigation we have identified is to guide the programmer to good paths
through the state space using techniques such as Feature-specific
Profiling@~cite[saf-cc-2015] with contracts/casts as the target feature.

We also intend to investigate the use of tracing JIT compilation based on the Pycket
work by @citet[bauman-et-al-icfp-2015]. The Pycket authors report dramatic reductions
in contract checking overhead in untyped Racket programs. We are interested in seeing
if tracing also benefits the kinds of contract usages that we see in gradually typed
programs.

@section{Conclusion}

Runtime overhead for gradually-typed programs is a pressing concern as gradual typing
is adopted both by researchers and by industrial groups. However, there are open
questions in both diagnosing where these overheads occur and in solving them by
reducing overheads or avoiding them. We propose a methodology for diagnosing such
overheads by visualizing how adding types to existing programs affects the runtime
along various gradual typing paths. Using the diagnostic information, we hope to
drive efforts in both tooling and compilation for gradually typed languages.

@generate-bibliography[]
