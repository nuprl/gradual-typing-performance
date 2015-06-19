#lang racket/base

(require benchmark-util
         (only-in racket/file file->lines file->string))

(require "lcs.rkt")

(define LARGE_TEST "../base/prufock.txt")
(define SMALL_TEST "../base/hunt.txt")

(define (self-test)
  (define txt (file->string "../base/code-sample.rkt"))
  (longest-common-substring txt txt)
  (void))

(define (main testfile)
  (define lines (file->lines testfile))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

;(time (main SMALL_TEST)) ;150ms
(time (main LARGE_TEST)) ; 2500ms
;(time (self-test)) ; 440ms
