#lang typed/racket/base

(provide:
  (ensure-dir
   (-> Path-String Void))
  ;; Check whether a directory exists.
  ;; If not, create it.

)

;; =============================================================================

(: ensure-dir (-> Path-String Void))
(define (ensure-dir path)
  (unless (directory-exists? path)
    (make-directory path)))

;; =============================================================================

(module+ test
  (require rackunit)
)
