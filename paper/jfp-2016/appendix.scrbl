#lang scribble/base
@require["common.rkt" "benchmark.rkt" "appendix.rkt" "util.rkt"]

@exact{\newpage}

@title[#:tag "appendix"]{Appendix}
@; =============================================================================
@profile-point{appendix:bib}
@section{Bibliography of Performance Costs}

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
                 #:author "John Griffin"
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


@; =============================================================================
@exact{\newpage}
@profile-point{appendix:module-graph}
@section{The @|GTP| Benchmarks, by Module}

The following summaries describe the module-level structure of benchmarks in the @|GTP| suite.
In particular, the summaries include:
@itemlist[
  @item{
    the name and size of each module;
  }
  @item{
    whether each module has an adaptor;
  }
  @item{
    the number of identifiers imported and exported by the module;
  }
  @item{
    and a graph of inter-module dependencies, with edges from each module to the modules it imports from.
  }
]
@; + miscellaneous stats, like number of wraps?

Modules are ordered alphabetically.
@Figure-ref{fig:suffixtree-lattice} uses this ordering to represent configurations as black and white rectangles.
For example, the node in @figure-ref{fig:suffixtree-lattice} in which only the left-most segment is white represents the configuration where module @tt{data.rkt} is untyped and all other modules are typed.
Similarly, @figure-ref{fig:exact-runtimes} derives a natural number for each configuration using the alphabetical order of module names.
Configuration 4 in @figure-ref{fig:exact-runtimes} (binary: @tt{0100}) is the configuration where only @tt{main.rkt} is typed.

@; MODULEGRAPHS
@; and use a graph structure to represent the interactions
@;  of its modules.
@; Nodes in the graphs represent modules in the program that our experiment
@;  varies as typed or untyped.
@; Edges represent static import statements.
@; For example, the leftmost node in each graph represents the program's main module,
@;  which imports from other modules but is never itself imported.
@; Finally, THEGUYS color and thicken each edge in proportion to the runtime cost
@;  associated with the edge.
@; @todo{what are colors/what mean?}
@; 

@exact|{\vspace{2ex}}|
@render-module-descriptions[
  @module-description[sieve #:scale 0.2 #:cache? #t
    @module["main" #f]{
      Computes prime numbers.
    }
    @module["streams" #f]{
      Implements functional streams.
    }
  ]

  @module-description[forth #:scale 0.5 #:cache? #t
    @module["command" #f]{
      Class and instances for calculator commands.
    }
    @module["eval" #f]{
      Forth evaluator.
    }
    @module["main" #f]{
      Runs the evaluator 
    }
    @module["stack" #f]{
      Functional stack, represents calculator state.
    }
  ]

  @module-description[acquire #:scale 0.9 #:cache? #t
    @module["admin" #f]{
      Register and manage game players.
    }
    @module["auxiliaries" #f]{
      Utility functions.
    }
    @module["basics" #f]{
      Defines basic game structures.
    }
    @module["board" #t]{
      Implements the game board and tiles.
    }
    @module["main" #f]{
      Starts a game of {\tt acquire}.
    }
    @module["player" #f]{
      Defines player class and three instances.
    }
    @module["state" #t]{
      Represents the internal game state.
    }
    @module["strategy" #f]{
      Helpers for implementing player instances.
    }
    @module["tree" #t]{
      State machine for managing game players.
    }
  ]

  @module-description[dungeon #:scale 0.4 #:cache? #t
    @module["cell" #f]{
      Class for maze cells.
    }
    @module["grid" #f]{
      An array of cells.
    }
    @module["main" #f]{
      Generates a maze.
    }
    @module["message-queue" #f]{
      Buffer for log messages.
    }
    @module["utils" #f]{
      Helpers for formatting and random sampling.
    }
  ]

  @module-description[fsm #:scale 0.3 #:cache? #t
    @module["automata" #t]{
      Represents actors in the economy.
    }
    @module["main" #f]{
      Driver module.
    }
    @module["population" #f]{
      A population of automata.
    }
    @module["utilities" #f]{
      Helper functions.
    }
  ]

  @module-description[fsmoo #:scale 0.3 #:cache? #t
    @module["automata" #t]{
      Class and instances for economy members.
    }
    @module["main" #f]{
      Driver module.
    }
    @module["population" #f]{
      Class representing an economy.
    }
    @module["utilities" #f]{
      Helper functions.
    }
  ]

  @module-description[gregor #:scale 0.9 #:cache? #t
    @module["clock" #f]{
      Interface to the system clock.
    }
    @module["core-structs" #t]{
      Struct definitions for years and hours.
    }
    @module["date" #f]{
      Represents calendar dates.
    }
    @module["datetime" #f]{
      Combined date and time.
    }
    @module["difference" #f]{
      Represents time deltas.
    }
    @module["gregor-structs" #t]{
      Struct definitions for date and time.
    }
    @module["hmsn" #f]{
      Conversions and constants for hours, minutes, etc.
    }
    @module["main" #f]{
      Creates date and time objects, runs unit tests.
    }
    @module["moment-base" #f]{
      Helpers for {\tt moment} and {\tt offset-resolvers}.
    }
    @module["moment" #f]{
      User interface to moments in time.
    }
    @module["offset-resolvers" #f]{
      API for resolving time deltas in a given time zone.
    }
    @module["time" #f]{
      Represents abstract times.
    }
    @module["ymd" #f]{
      Year-month-day structures.
    }
  ]

  @module-description[kcfa #:scale 0.7 #:cache? #t
    @module["ai" #f]{
      Abstract interpreter for CPS programs.
    }
    @module["benv" #t]{
      Implements a binding environment.
    }
    @module["denotable" #t]{
      Denotation of closures, in the abstract interpreter.
    }
    @module["main" #f]{
      Creates and program and invokes 1-CFA.
    }
    @module["structs" #t]{
      Core struct definitions.
    }
    @module["time" #t]{
      Represents time during abstract interpretation.
    }
    @module["ui" #f]{
      API to the abstract interpreter.
    }
  ]

  @module-description[lnm #:scale 0.4 #:cache? #t
    @module["bitstring" #f]{
      Implements binary numbers.
    }
    @module["lnm-plot" #f]{
      Builds overhead plots.
    }
    @module["main" #f]{
      Driver module.
    }
    @module["modulegraph" #t]{
      Represents benchmark programs.
    }
    @module["spreadsheet" #f]{
      Exports a dataset to CSV format.
    }
    @module["summary" #t]{
      Collected data for a benchmark program.
    }
  ]

  @module-description[mbta #:scale 0.3 #:cache? #t
    @module["main" #f]{
      Queries a subway map.
    }
    @module["run-t" #f]{
      Query-processing server.
    }
    @module["t-graph" #f]{
      Represents a subway map.
    }
    @module["t-view" #f]{
      Manages a subway map.
    }
  ]

  @module-description[morsecode #:scale 0.3 #:cache? #t
    @module["levenshtein" #f]{
      Distance metric for strings.
    }
    @module["main" #f]{
      Driver module.
    }
    @module["morse-code-strings" #f]{
      Converts a string to morse code.
    }
    @module["morse-code-table" #f]{
      Map from characters to dots and dashes.
    }
  ]

  @module-description[quadBG #:scale 0.9 #:cache? #t
    @module["hyphenate" #f]{
      Knuth--Liang hyphenation algorithm.
    }
    @module["main" #f]{
      Typesets a small document.
    }
    @module["measure" #f]{
      Measures the size of text.
    }
    @module["ocm-struct" #t]{
      Represents one part of a document.
    }
    @module["ocm" #f]{
      Combines parts of a document.
    }
    @module["penalty-struct" #t]{
      Measures the ``cost'' of hyphenating a word.
    }
    @module["quad-main" #f]{
      Typesetting API.
    }
    @module["quads" #f]{
      Basic data structures.
    }
    @module["quick-sample" #f]{
      Source code for a small document.
    }
    @module["render" #f]{
      Builds a PDF.
    }
    @module["sugar-list" #f]{
      List helper functions.
    }
    @module["utils" #f]{
      Quad helper functions.
    }
    @module["world" #f]{
      Constant definitions.
    }
    @module["wrap" #f]{
      Adds horizontal padding to a line of text.
    }
  ]

  @module-description[quadMB #:scale 0.9 #:cache? #t
    @module["exceptions" #f]{
      Irregular hyphenation patterns.
    }
    @module["hyphenate" #f]{
      Knuth--Liang hyphenation algorithm.
    }
    @module["main" #f]{
      Typesets a small document.
    }
    @module["measure" #f]{
      Measures the size of text.
    }
    @module["ocm-struct" #t]{
      Represents one part of a document.
    }
    @module["ocm" #f]{
      Combines parts of a document.
    }
    @module["patterns-hashed" #f]{
      Map of hyphenation patterns.
    }
    @module["penalty-struct" #t]{
      Measures the ``cost'' of hyphenating a word.
    }
    @module["quad-main" #f]{
      Typesetting API.
    }
    @module["quads" #f]{
      Basic data structures.
    }
    @module["quick-sample" #f]{
      Source code for a small document.
    }
    @module["render" #f]{
      Builds a PDF.
    }
    @module["sugar-list" #f]{
      List helper functions.
    }
    @module["utils" #f]{
      Quad helper functions.
    }
    @module["world" #f]{
      Constant definitions.
    }
    @module["wrap" #f]{
      Adds horizontal padding to a line of text.
    }
  ]

  @module-description[snake #:scale 0.7 #:cache? #t
    @module["collide" #f]{
      Collision detection.
    }
    @module["const" #f]{
      Constant definitions.
    }
    @module["cut-tail" #f]{
      Shorten the game's snake.
    }
    @module["data" #t]{
      Structure definitions.
    }
    @module["handlers" #f]{
      Handle key events, detect game over.
    }
    @module["main" #f]{
      Driver module.
    }
    @module["motion-help" #f]{
      Move segments of a snake, spawn food.
    }
    @module["motion" #f]{
      Advance the game state.
    }
  ]

  @module-description[suffixtree #:scale 0.7 #:cache? #t
    @module["data" #t]{
      Structure definitions.
    }
    @module["label" #f]{
      Sharable, sliceable strings.
    }
    @module["lcs" #f]{
      Longest common subsequence.
    }
    @module["main" #f]{
      Driver module.
    }
    @module["structs" #f]{
      Suffix tree functions.
    }
    @module["ukkonen" #f]{
      Ukkonen's suffix tree algorithm.
    }
  ]

  @module-description[synth #:scale 0.9 #:cache? #t
    @module["array-broadcast" #f]{
      Re-shapes an array.
    }
    @module["array-struct" #f]{
      Low-level array operations.
    }
    @module["array-transform" #f]{
      Combines arrays.
    }
    @module["array-utils" #f]{
      Utility functions.
    }
    @module["data" #t]{
      Common data structures.
    }
    @module["drum" #f]{
      Creates a drum beat.
    }
    @module["main" #f]{
      Driver module.
    }
    @module["mixer" #f]{
      Converts floating-point signals into music.
    }
    @module["sequencer" #f]{
      Creates a WAV file.
    }
    @module["synth" #f]{
      Implements a synthesizer.
    }
  ]

  @module-description[take5 #:scale 0.7 #:cache? #t
    @module["basics" #f]{
      Constant definitions.
    }
    @module["card-pool" #f]{
      Players' shared cards.
    }
    @module["card" #t]{
      Struct for game cards.
    }
    @module["dealer" #f]{
      Class for managing players.
    }
    @module["deck" #f]{
      The dealer's deck of cards.
    }
    @module["main" #f]{
      Starts a game.
    }
    @module["player" #f]{
      Class for game players.
    }
    @module["stack" #f]{
      Internal representation of the card deck.
    }
  ]

  @module-description[tetris #:scale 0.9 #:cache? #t
    @module["aux" #f]{
      Helper functions.
    }
    @module["block" #f]{
      One square on a tetris board.
    }
    @module["bset" #f]{
      A set of blocks.
    }
    @module["consts" #f]{
      Constant definitions.
    }
    @module["data" #t]{
      Structure definitions.
    }
    @module["elim" #f]{
      Delets a row of blocks.
    }
    @module["main" #f]{
      Driver module.
    }
    @module["tetras" #f]{
      A connected group of blocks.
    }
    @module["world" #f]{
      Tetris game state.
    }
  ]

  @module-description[zombie #:scale 0.3 #:cache? #t
    @module["image" #t]{
      Defines placeholder image.
    }
    @module["main" #f]{
      Driver module.
    }
    @module["math" #f]{
      Helper functions.
    }
    @module["zombie" #f]{
      Implements game entities as higher-order functions.
    }
  ]

  @module-description[zordoz #:scale 0.4 #:cache? #t
    @module["main" #f]{
      Driver module.
    }
    @module["zo-find" #f]{
      Searches bytecode structures.
    }
    @module["zo-shell" #f]{
      API to explore bytecode.
    }
    @module["zo-string" #f]{
      Prints a bytecode structure.
    }
    @module["zo-transition" #f]{
      Traverses bytecode structures.
    }
  ]

]

@profile-point{appendix:end}
