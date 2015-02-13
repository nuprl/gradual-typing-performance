#lang racket/base

(require (only-in "zo-shell.rkt" init))

;; Stress tests: search entire bytecode for the fairly-common branch struct
;(define BYTECODE '("../base/zo-shell.zo" "../base/zo-find.zo" "../base/zo-string.zo" "../base/zo-transition.zo"))
(define BYTECODE '("../base/hello-world.zo"))
(define (main)
  (for ([b BYTECODE]) (init (vector b "branch"))))

(time (with-output-to-file "/dev/null" main #:exists 'append))
