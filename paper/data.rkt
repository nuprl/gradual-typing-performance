#lang racket/base

;; Canonical data files for each benchmark

(define-syntax-rule (define+provide [name value] ...)
  (begin
    (define name value) ...
    (provide name ...)))

(define+provide
  [GREGOR-DATA     "./data/gregor-2015-07-26T05:27:18.rktd"]
  [KCFA-DATA       "./data/kcfa-2015-06-25T13:48:52.rktd"]
  [LNM-DATA        "./data/lnm-large-06-28.rktd"]
  [MBTA-DATA       "./data/mbta-2015-09-07T23:17:52.rktd"]
  [MORSECODE-DATA  "./data/morsecode-large-06-27.rktd"]
  [QUAD-DATA       "./data/quad-galicia-07-26.rktd"]
  [SIEVE-DATA      "./data/sieve-2015-06-28T14:08:55.rktd"]
  [SNAKE-DATA      "./data/snake-2015-06-30T14:55:40.rktd"]
  [SUFFIXTREE-DATA "./data/suffixtree-large-06-30.rktd"]
  [SYNTH-DATA      "./data/synth-2015-07-02T01:47:43.rktd"]
  [TETRIS-DATA     "./data/tetris-2015-07-01T16:39:46.rktd"]
  [ZORDOZ-DATA     "./data/zordoz-04-09.rktd"]
)
