#lang typed/racket/base

(require (only-in "zo-shell.rkt" init))

;; Stress tests: search entire bytecode for the fairly-common branch struct
(: BYTECODE (Listof String))
(define BYTECODE '("zo-shell.zo" "zo-find.zo" "zo-string.zo" "zo-transition.zo"))
(: main (-> Void))
(define (main)
  (for : Void ([b : String BYTECODE]) (init (vector b "branch"))))

(time (with-output-to-file "/dev/null" main #:exists 'append))
