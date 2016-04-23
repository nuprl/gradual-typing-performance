#lang typed/racket/base

(require benchmark-util
 (only-in racket/file file->lines file->string))

(require/typed/check "lcs.rkt"
  [longest-common-substring (-> String String String)])

(define MICRO_TEST "../base/micro.txt")
(define SMALL_TEST "../base/hunt.txt")
(define LARGE_TEST "../base/prufock.txt")
(define KCFA_TYPED "../base/kcfa-typed.rkt")

;; LCS on all pairs of lines in a file
(: main (-> String Void))
(define (main testfile)
  (define lines (file->lines testfile))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

;(time (main MICRO_TEST))
(time (main SMALL_TEST)) ; 110ms
;(time (main LARGE_TEST)) ; 1900ms
;(time (main KCFA_TYPED)) ; 16235ms
