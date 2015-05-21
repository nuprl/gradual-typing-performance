#lang racket/base

(provide node-find-child)

;; -----------------------------------------------------------------------------

(require "data-node.rkt"
         "data-label.rkt"
(only-in "label-label-ref.rkt"
  label-ref)
(only-in "label-label-element-equal.rkt"
  label-element-equal?))

;; =============================================================================

;; node-find-child: node label-element -> (union node #f)
;;
;; Finds the first child node whose up-label starts with (label-ref
;; label 0).  If none can be found, returns #f.
(define (node-find-child node label-element)
  (define (loop children)
    (cond ((null? children) #f)
          ((label-element-equal? label-element (label-ref (node-up-label (car children)) 0))
           (car children))
          (else
           (loop (cdr children)))))
  (loop (node-children node)))
