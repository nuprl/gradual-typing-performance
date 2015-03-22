#lang typed/racket/base

(require benchmark-util
 (only-in racket/file file->lines))

(require/typed/check "lcs.rkt"
  [longest-common-substring (-> String String String)])

(define LARGE_TEST "../base/prufock.txt")
(define SMALL_TEST "../base/hunt.txt")

(define (main)
  (define lines (file->lines LARGE_TEST))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

(time (main))
