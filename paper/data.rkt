#lang racket/base

;; Canonical data files for each benchmark

(define-syntax-rule (define+provide [name value] ...)
  (begin
    (define name value) ...
    (provide name ...)))

(define+provide
  [GREGOR-DATA     "./data/gregor-2015-06-30.rktd"]
  [KCFA-DATA       "./data/kcfa-2015-06-25T13:48:52.rktd"]
  [LNM-DATA        "./data/lnm-large-06-28.rktd"]
  [MBTA-DATA       "./data/mbta-04-25.rktd"]
  [MORSECODE-DATA  "./data/morsecode-large-06-27.rktd"]
  [QUAD-DATA       "./data/quad-placeholder.rktd"]
  [SIEVE-DATA      "./data/sieve-2015-06-28T14:08:55.rktd"]
  [SNAKE-DATA      "./data/snake-2015-06-30T14:55:40.rktd"]
  [SUFFIXTREE-DATA "./data/suffixtree-06-10.rktd"]
  [SYNTH-DATA      "./data/funkytown-2015-06-30T15:34:39.rktd"]
  [TETRIS-DATA     "./data/tetris-large-06-20.rktd"]
  [ZORDOZ-DATA     "./data/zordoz-04-09.rktd"]
)
