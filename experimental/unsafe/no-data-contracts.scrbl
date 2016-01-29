#lang scribble/sigplan @onecolumn 

@; WARNING: this file does not compile!
@;  Need to move it, and the module graphs, to the paper/ folder.
@;  (Mostly because it's expecting the scripts/ and data/ folders)

@; @bold{Observations: }
@; @itemlist[
@; @item{Max. & Mean overheads are much better (50x, 10x)}
@; @item{Usability is consistently better, but still not very good. Less than 60% are 300-usable}
@; ]
@; 
@; @bold{Conclusion: }
@; "perfect" data access will be a huge improvement,
@; but it may not help anyone, in practice.

@(require "common.rkt"
  "render-lnm.rkt" "data.rkt")

@figure*["fig:datafree" "Without data contracts"
  @(let* ([data `(("gregor"       "unsafe/ugregor.rktd")
                  ("snake"        "unsafe/usnake.rktd")
                  ("suffixtree"   "unsafe/usuffixtree.rktd")
                  ("tetris"       "unsafe/utetris.rktd")
                  ("synth"        "unsafe/usynth.rktd"))])
     (data->pict data #:tag "3"))
]

@figure*["fig:orig" "Original module graphs"
  @(let* ([data `(("gregor"       ,GREGOR-DATA)
                  ("snake"        ,SNAKE-DATA)
                  ("suffixtree"   ,SUFFIXTREE-DATA)
                  ("tetris"       ,TETRIS-DATA)
                  ("synth"        ,SYNTH-DATA))])
     (data->pict data #:tag "4"))
]

