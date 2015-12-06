#lang scribble/sigplan @onecolumn 

@(require "common.rkt")

@authorinfo["Asumu Takikawa, 
             Daniel Feltey,
             Ben Greenman,
             Max S. New,
             Jan Vitek,
             Matthias Felleisen" "Northeastern University, Boston, USA" ""]

@title{Is Sound Gradual Typing Dead?}

@abstract{Programmers have come to embrace dynamically-typed languages for
 prototyping and delivering large and complex systems. When it comes to
 maintaining and evolving these systems, the lack of explicit static typing
 becomes a bottleneck. In response, researchers have explored the idea of
 gradually-typed programming languages which allow the incremental addition of
 type annotations to software written in one of these untyped languages.
 Some of these new, hybrid languages insert run-time checks at the boundary
 between typed and untyped code to establish type soundness for the overall
 system. With sound gradual typing, programmers can rely on the language
 implementation to provide meaningful error messages when type invariants
 are violated.
 While most research on sound gradual typing remains theoretical, the
 few emerging implementations suffer from performance overheads due to these
 checks. None of the publications on this topic comes with a
 comprehensive performance evaluation. Worse, a few report disastrous numbers.

 In response, this paper proposes a method for
 evaluating the performance of gradually-typed programming languages.
 The method hinges on exploring the space of partial conversions from
 untyped to typed. For each benchmark, the performance of the different
 versions is reported in a synthetic metric that associates runtime overhead
 to conversion effort.
 The paper reports on the results of applying the method to
 Typed Racket, a mature implementation of sound gradual typing, using a suite
 of real-world programs of various sizes and complexities.  Based on these
 results the paper concludes that, given the current state
 of implementation technologies, sound gradual typing faces significant
 challenges. Conversely, it raises the question of how implementations could 
 reduce the overheads associated with soundness and how
 tools could be used to steer programmers clear from pathological cases.}

@category["D.2.8" "Software Engineering" "Metrics" "Performance measures"]

@; Cut for space
@;@category["D.3.3" "Programming Languages" "Language Constructs and Features"]
@;@terms{Performance, Experimentation, Measurement}

@keywords{Gradual typing, performance evaluation}

@include-section{intro.scrbl}
@include-section{framework.scrbl}
@include-section{benchmarks.scrbl}
@include-section{typed-racket.scrbl}
@;include-section{pycket.scrbl}
@include-section{death.scrbl}
@include-section{related.scrbl}
@include-section{conclusion.scrbl}

@section[#:style 'unnumbered]{Acknowledgments}

The authors gratefully acknowledge support from the National Science
Foundation (SHF 1518844). They also thank Matthew Butterick, John Clements,
Matthew Might, Vincent St-Amour, Neil Toronto, David Van Horn, Danny Yoo, and Jon
Zeppieri for providing benchmark code bases. Brian LaChance and Sam
Tobin-Hochstadt provided valuable feedback on earlier drafts.

@generate-bibliography[#:sec-title "References"]
