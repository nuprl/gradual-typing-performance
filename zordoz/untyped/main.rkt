#lang racket/base

(require benchmark-util)

(require (only-in "zo-shell.rkt"
  init))

;; Stress tests: search entire bytecode for the fairly-common branch struct
;; (define SELF-TEST '("../base/zo-find.zo")); "../base/zo-shell.zo")); "../base/zo-string.zo" "../base/zo-transition.zo"))
;; (define (self-test)
;;   (for ([b SELF-TEST]) (init (vector b "branch"))))

(define SMALL-TEST "../base/hello-world.zo")
(define (small-test)
  (init (vector SMALL-TEST "branch")))

(define MID-TEST "../base/array-struct.zo")
(define (mid-test)
  (init (vector LARGE-TEST "branch")))

(define LARGE-TEST "../base/streams.zo")
(define (large-test)
  (init (vector LARGE-TEST "branch")))

;; -----------------------------------------------------------------------------

(define-syntax-rule (main test)
  (with-output-to-file "/dev/null" test #:exists 'append))

;(time (main self-test)) ; 1314967ms
;(time (main small-test)) ; 6ms
;(time (main mid-test)) ; 343ms
(time (main large-test)) ; 26343ms
