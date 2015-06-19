#lang typed/racket/base

(require benchmark-util
 (only-in racket/file file->lines file->string))

(require/typed/check "lcs.rkt"
  [longest-common-substring (-> String String String)])

(define LARGE_TEST "../base/prufock.txt")
(define SMALL_TEST "../base/hunt.txt")

(define (self-test)
  (define txt (file->string "../base/code-sample.rkt"))
  (longest-common-substring txt txt)
  (void))

(: main (-> String Void))
(define (main testfile)
  (define lines (file->lines testfile))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

;(time (main SMALL_TEST)) ; 100ms
(time (main LARGE_TEST)) ; 1800ms
;(time (self-test)) ; 390ms
