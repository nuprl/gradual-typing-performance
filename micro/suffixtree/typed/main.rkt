#lang typed/racket/base

;; -----------------------------------------------------------------------------

(require benchmark-util
 (only-in racket/file file->lines))
(require/typed/check "lcs-longest-common-substring.rkt"
  [longest-common-substring (-> String String String)])

;; =============================================================================

(: main (-> Path-String Void))
(define (main test-file)
  (define lines (file->lines test-file))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

(define LARGE_TEST "../base/prufock.txt")
(define SMALL_TEST "../base/hunt.txt")

(time (main SMALL_TEST))
