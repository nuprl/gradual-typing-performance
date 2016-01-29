#lang racket/base

(provide node-follow/k)

;; -----------------------------------------------------------------------------

(require "data-node.rkt"
         "data-label.rkt"
(only-in "label-label-ref.rkt"
  label-ref)
(only-in "label-label-length.rkt"
  label-length)
(only-in "label-label-copy.rkt"
  label-copy)
(only-in "label-label-element-equal.rkt"
  label-element-equal?)
(only-in "structs-node-find-child.rkt"
  node-find-child))

;; =============================================================================

;; node-follow/k: node label (node -> A)
;;                           (node number -> B)
;;                           (node label number -> C)
;;                           (node number label number -> D)
;;                    -> (union A B C D)
;;
;; Traverses the node's edges along the elements of the input label.
;; Written in continuation-passing-style for leakage containment.
;; One of the four continuation arguments will be executed.
(define (node-follow/k node
                       original-label
                       matched-at-node/k
                       matched-in-edge/k
                       mismatched-at-node/k
                       mismatched-in-edge/k)
  (define (EDGE/k node label label-offset)
    (define up-label (node-up-label node))
    (let loop ((k 0))
      (define k+label-offset (+ k label-offset))
      (cond
       ((= k (label-length up-label))
        (unless (integer? k+label-offset) (error "node/folllowd"))
        (NODE/k node label k+label-offset))
       ((= k+label-offset (label-length label))
        (unless (integer? k) (error "node/followk"))
        (matched-in-edge/k node k))
       ((label-element-equal? (label-ref up-label k)
                              (label-ref label k+label-offset))
        (loop (add1 k)))
       (else
        (unless (and (integer? k)
                     (integer? k+label-offset)) (error "node-follow/k mismatched fail"))
        (mismatched-in-edge/k node k label
                              k+label-offset)))))
  (define (NODE/k node label label-offset)
    (if (= (label-length label) label-offset)
        (matched-at-node/k node)
        (let ([child (node-find-child node (label-ref label label-offset))])
          (if child
              (EDGE/k child label label-offset)
              (mismatched-at-node/k node label label-offset)))))
  (NODE/k node (label-copy original-label) 0))
