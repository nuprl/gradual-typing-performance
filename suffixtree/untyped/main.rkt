#lang racket/base

(require
  "util.rkt"
  (only-in racket/file file->lines))

(define LARGE_TEST "../base/prufock.txt")
(define SMALL_TEST "../base/hunt.txt")

(define (main)
  (define lines (file->lines LARGE_TEST))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

(time (main))
