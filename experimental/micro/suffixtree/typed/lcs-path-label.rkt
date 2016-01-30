#lang typed/racket/base

(provide path-label)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt"
         "data-label-adapted.rkt"
         "data-suffix-tree-adapted.rkt"
         benchmark-util)
(require/typed/check "label-label-length.rkt"
  [label-length (-> label Index)])
(require/typed/check "label-vector-label.rkt"
  [vector->label (-> (Vectorof (U Char Symbol)) label)])
(require/typed/check "label-label-ref.rkt"
  [label-ref (-> label Index (U Char Symbol))])

;; =============================================================================

;; path-label: node -> label
;;
;; Returns a new label that represents the path from the tree root
;; to this node.
;;
;; Fixme: optimize the representation of label to be able to do this
;; without much reallocation.  Maybe another label class that uses a
;; rope data structure might be better...  I need to read Hans
;; Boehm's paper on "Ropes, an alternative to strings" to see how
;; much work this would be.
(: path-label (-> Node Label))
(define (path-label node)
  (: collect-loop (-> (U Node #f) (Listof Label) Integer Label))
  (define (collect-loop current-node collected-labels total-length)
    (if current-node
      (collect-loop (node-parent current-node)
                    (cons (node-up-label current-node) collected-labels)
                    (+ total-length
                       (label-length (node-up-label current-node))))
      (build-new-label collected-labels total-length)))
  (: vector-blit! (-> Label (Vectorof (U Char Symbol)) Index Void))
  (define (vector-blit! src-label dest-vector dest-offset)
    (let loop ((i 0))
      (let ([index (+ i dest-offset)])
      (when (and (< i (label-length src-label)) (index? i) (index? index))
        (vector-set! dest-vector
                     index
                     (label-ref src-label i))
        (loop (add1 i))))))
  (: build-new-label (-> (Listof Label) Integer Label))
  (define (build-new-label labels total-length)
    (: vector (Vectorof (U Char Symbol)))
    (define vector (make-vector total-length 'X))
    (let loop ((labels labels) (i 0))
      (cond [(null? labels)
             (vector->label vector)]
            [(index? i)
              (vector-blit! (car labels) vector i)
              (loop (cdr labels)
                    (+ i (label-length (car labels))))]
            [else (error "not an index")])))
  (collect-loop node '() 0))
