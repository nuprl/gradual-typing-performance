#lang typed/racket/base

(provide node-find-child)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt"
         "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-label-ref.rkt"
  [label-ref (-> label Integer (U Char Symbol))])
(require/typed/check "label-label-element-equal.rkt"
  [label-element-equal? (-> Any Any Boolean)])

;; =============================================================================

;; node-find-child: node label-element -> (union node #f)
;;
;; Finds the first child node whose up-label starts with (label-ref
;; label 0).  If none can be found, returns #f.
(: node-find-child (-> Node Any (U Node #f)))
(define (node-find-child node label-element)
  (: loop (-> (Listof Node) (U Node #f)))
  (define (loop children)
    (cond ((null? children) #f)
          ((label-element-equal? label-element (label-ref (node-up-label (car children)) 0))
           (car children))
          (else
           (loop (cdr children)))))
  (loop (node-children node)))
