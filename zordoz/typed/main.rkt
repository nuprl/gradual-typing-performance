#lang typed/racket/base

(require benchmark-util)

(require/typed/check "zo-shell.rkt"
  [init (-> (Vectorof String) Void)])

;; Stress tests: search entire bytecode for the fairly-common branch struct
(: BYTECODE (Listof String))
;(define BYTECODE '("../base/zo-shell.zo" "../base/zo-find.zo" "../base/zo-string.zo" "../base/zo-transition.zo"))
(define BYTECODE '("../base/hello-world.zo"))
(: main (-> Void))
(define (main)
  (for : Void ([b : String BYTECODE]) (init (vector b "branch"))))

(time (with-output-to-file "/dev/null" main #:exists 'append))
