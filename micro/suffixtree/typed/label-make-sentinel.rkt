#lang typed/racket/base

(provide make-sentinel)

;; -----------------------------------------------------------------------------
;; =============================================================================

(: make-sentinel (-> Symbol))
(define (make-sentinel)
 (gensym 'sentinel))
