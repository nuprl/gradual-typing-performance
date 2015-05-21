#lang typed/racket/base

(provide label-prefix?)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-label-adapted.rkt")
(require/typed/check "label-label-length.rkt"
  [label-length (-> label Index)])
(require/typed/check "label-label-ref.rkt"
  [label-ref (-> label Integer (U Symbol Char))])


;; =============================================================================

;; label-prefix?: label label -> boolean
;; Returns true if the first label is a prefix of the second label
(: label-prefix? (-> label label Boolean))
(define (label-prefix? prefix other-label)
  (let ((m (label-length prefix))
        (n (label-length other-label)))
    (if (> m n)                       ; <- optimization: prefixes
                                        ; can't be longer.
        #f
        (let loop ((k 0))
          (if (= k m)
              #t
              (and (index? k)
                   (equal? (label-ref prefix k) (label-ref other-label k))
                   (loop (add1 k))))))))
