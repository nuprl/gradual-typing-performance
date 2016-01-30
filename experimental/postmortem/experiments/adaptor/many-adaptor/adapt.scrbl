#lang scribble/sigplan @onecolumn

;; Pre-Requisites:
;; - A file `data.rkt` with variables pointing to the paper's experimental data
;; - Data files for gregor, suffixtree, and synth in the `data/` directory.
;; - Module graphs for the original and updated benchmarks, in a `module-graph/`
;;   folder.

;; AKA, paste this file into the `paper/` directory, and also paste in the
;;  module graphs & .rktd files.

@(require "common.rkt"
  "render-lnm.rkt" "data.rkt")

@figure*["fig:st" "Suffixtree"
  @(let* ([data `(
                  ("1 adaptor"   ,SUFFIXTREE-DATA)
                  ("2 adaptor"   "data/adapt+suffixtree-2015-08-25.rktd"))])
     (data->pict data #:tag "3"))
]

@figure*["fig:sy" "Synth"
  @(let* ([data `(
                  ("With data.rkt"  ,SYNTH-DATA)
                  ("Inline data"   "data/adapt+synth-2015-08-26.rktd"))])
     (data->pict data #:tag "4"))
]

@figure*["fig:gr" "Gregor"
  @(let* ([data `(
                  ("1 adaptor"   ,GREGOR-DATA)
                  ("2 adaptor"   "data/adapt+gregor-2015-08-28.rktd"))])
     (data->pict data #:tag "5"))
]


