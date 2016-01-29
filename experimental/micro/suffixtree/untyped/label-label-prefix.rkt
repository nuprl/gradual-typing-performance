#lang racket/base

(provide label-prefix?)

;; -----------------------------------------------------------------------------

(require
         "data-label-adapted.rkt"
(only-in "label-label-length.rkt" label-length)
(only-in "label-label-ref.rkt" label-ref))


;; =============================================================================

;; label-prefix?: label label -> boolean
;; Returns true if the first label is a prefix of the second label
(define (label-prefix? prefix other-label)
  (let ((m (label-length prefix))
        (n (label-length other-label)))
    (if (> m n)                       ; <- optimization: prefixes
                                        ; can't be longer.
        #f
        (let loop ((k 0))
          (if (= k m)
              #t
              (and (integer? k)
                   (equal? (label-ref prefix k) (label-ref other-label k))
                   (loop (add1 k))))))))
