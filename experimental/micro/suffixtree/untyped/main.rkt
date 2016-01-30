#lang racket/base

;; -----------------------------------------------------------------------------

(require
 (only-in racket/file file->lines)
 (only-in "lcs-longest-common-substring.rkt"
  longest-common-substring))

;; =============================================================================

(define (main test-file)
  (define lines (file->lines test-file))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

(define LARGE_TEST "../base/prufock.txt")
(define SMALL_TEST "../base/hunt.txt")

(time (main SMALL_TEST))
