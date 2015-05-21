#lang racket/base

(provide find-next-extension-point/add-suffix-link!)

;; -----------------------------------------------------------------------------

(require "data-node.rkt"
         "data-label.rkt"
(only-in "label-label-length.rkt"
  label-length)
(only-in "label-label-ref.rkt"
  label-ref)
(only-in "label-label-element-equal.rkt"
  label-element-equal?)
(only-in "structs-node-position-at-end.rkt"
  node-position-at-end?)
(only-in "structs-node-find-child.rkt"
  node-find-child)
(only-in "ukkonen-try-to-set-suffix-edge.rkt"
  try-to-set-suffix-edge!)
(only-in "ukkonen-jump-to-suffix.rkt"
  jump-to-suffix)
(only-in "ukkonen-skip-count-helper.rkt"
  skip-count-helper))

;; =============================================================================

;; find-next-extension-point/add-suffix-link!: node label number number ->
;;     (values node number number)
;;
;; Given the last active node where an extension was last made,
;; looks for the next position for extension.  Returns that
;; extension point's node and label offset, as well as the new phase
;; number i.  (Postcondition: (>= i initial-i))
;;
;; The first pass through the loop is a special case: we set the
;; suffix link from node to suffix-node unless we expect it to be
;; done from a splicing extension.
;;
;; If we run off the label (implicit tree), returns (values #f #f #f).
(define (find-next-extension-point/add-suffix-link! node label initial-i j)
  (define (fixed-start suffix-offset)
    (let ([i (if suffix-offset (- initial-i suffix-offset) j)])
      (unless (integer? i) (error "find-next")) i))
  (define-values (suffix-node suffix-offset)
    (jump-to-suffix node))
  (define K
    (fixed-start (cond [(integer? suffix-offset) suffix-offset]
                       [(eq? #t suffix-offset) 1]
                       [(eq? #f suffix-offset) #f]
                       [else (error "find-next")])))
  (define N
    (let ([i (label-length label)])
      (unless (integer? i) (error "find-next")) i))
   (define (loop-first i)
     (loop-general i (lambda (skipped-node
                              skip-offset)
                       (when (node-position-at-end? skipped-node skip-offset)
                         (try-to-set-suffix-edge! node skipped-node)))))
   (define (loop-rest i)
     (loop-general i (lambda (skipped-node skip-offset)
                       (void))))
   (define (loop-general i first-shot)
     (cond [(>= i N) (values #f #f #f)]
           [else
            (define-values (skipped-node skipped-offset)
              (skip-count-helper suffix-node label K i))
            (first-shot skipped-node skipped-offset)
             (if (node-position-at-end? skipped-node skipped-offset)
                 (find-extension-at-end! skipped-node skipped-offset i)
                 (find-extension-in-edge skipped-node skipped-offset i))]))
   (define (find-extension-in-edge skipped-node skip-offset i)
     (cond [(label-element-equal?
             (label-ref label i)
             (label-ref (node-up-label skipped-node) skip-offset))
            (let ([n (add1 i)]) (unless (integer? n) (error "find-next")) (loop-rest n))]
           [else (values skipped-node skip-offset i)]))
   (define (find-extension-at-end! skipped-node skip-offset i)
     (cond [(node-find-child skipped-node (label-ref label i))
            (let ([n (add1 i)]) (unless (integer? n) (error "find-next")) (loop-rest n))]
           [else (values skipped-node skip-offset i)]))
   (loop-first initial-i))
