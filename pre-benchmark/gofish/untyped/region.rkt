#lang racket/base

(provide
  make-hand-region
  make-discard-region
  make-discard-count-region
)

;; -----------------------------------------------------------------------------

(require
  racket/class
  "../base/cards.rkt"
)

;; =============================================================================

;; Region layout constants
(define SUBMARGIN 10)
(define LABEL-H 15)

;; Region-makers
(define (make-hand-region r cw ch)
  (define m SUBMARGIN)
  (make-region (+ m (region-x r)) (+ LABEL-H m (region-y r))
               (- (region-w r) (* 3 m) cw)
               (- (region-h r) LABEL-H (* 2 m))
               #f #f))

(define (make-discard-region r cw ch)
 (make-region (- (+ (region-x r) (region-w r)) SUBMARGIN cw)
              (- (+ (region-y r) (region-h r)) SUBMARGIN ch)
              cw ch
              #f #f))

(define (make-discard-count-region r c cb cw ch)
 (make-region
  (- (+ (region-x r) (region-w r)) SUBMARGIN cw (/ SUBMARGIN 2))
  (- (+ (region-y r) (region-h r)) SUBMARGIN ch LABEL-H (/ SUBMARGIN 2))
  (+ cw SUBMARGIN) (+ ch LABEL-H SUBMARGIN)
  (number->string c)
  cb))
