#lang racket/base

(require (only-in "zo-shell.rkt" init))

;; Stress tests: search entire bytecode for the fairly-common branch struct
(define BYTECODE '("zo-shell.zo" "zo-find.zo" "zo-string.zo" "zo-transition.zo"))
(define (main)
  (for ([b BYTECODE]) (init (vector b "branch"))))

(time (with-output-to-file "/dev/null" main #:exists 'append))
