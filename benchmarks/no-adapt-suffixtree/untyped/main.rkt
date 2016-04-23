#lang racket/base

(require benchmark-util
         (only-in racket/file file->lines file->string))

(require/check "lcs.rkt")

(define MICRO_TEST "../base/micro.txt")
(define LARGE_TEST "../base/prufock.txt")
(define SMALL_TEST "../base/hunt.txt")
(define KCFA_TYPED "../base/kcfa-typed.rkt")

;; LCS on all pairs of lines in a file
(define (main testfile)
  (define lines (file->lines testfile))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

;(time (main MICRO_TEST))
(time (main SMALL_TEST)) ;150ms
;(time (main LARGE_TEST)) ; 2500ms
;(time (main KCFA_TYPED)) ; 22567ms
