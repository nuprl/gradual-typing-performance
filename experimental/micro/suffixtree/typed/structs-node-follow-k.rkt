#lang typed/racket/base

(provide node-follow/k)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt"
         "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-label-ref.rkt"
  [label-ref (-> label Integer (U Char Symbol))])
(require/typed/check "label-label-length.rkt"
  [label-length (-> label Index)])
(require/typed/check "label-label-copy.rkt"
  [label-copy (-> label label)])
(require/typed/check "label-label-element-equal.rkt"
  [label-element-equal? (-> Any Any Boolean)])
(require/typed/check "structs-node-find-child.rkt"
  [node-find-child (-> Node Any (U Node #f))])

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
(: node-follow/k (All (A B C D)
                      (-> Node
                          Label
                          (-> Node A)
                          (-> Node Index B)
                          (-> Node Label Index C)
                          (-> Node Index Label Index D)
                          (U A B C D))))
(define (node-follow/k node
                       original-label
                       matched-at-node/k
                       matched-in-edge/k
                       mismatched-at-node/k
                       mismatched-in-edge/k)
  (: EDGE/k (-> Node Label Index (U A B C D)))
  (define (EDGE/k node label label-offset)
    (: up-label Label)
    (define up-label (node-up-label node))
    (let loop ((k 0))
      (define k+label-offset (+ k label-offset))
      (cond
       ((= k (label-length up-label))
        (unless (index? k+label-offset) (error "node/folllowd"))
        (NODE/k node label k+label-offset))
       ((= k+label-offset (label-length label))
        (unless (index? k) (error "node/followk"))
        (matched-in-edge/k node k))
       ((label-element-equal? (label-ref up-label k)
                              (label-ref label k+label-offset))
        (loop (add1 k)))
       (else
        (unless (and (index? k)
                     (index? k+label-offset)) (error "node-follow/k mismatched fail"))
        (mismatched-in-edge/k node k label
                              k+label-offset)))))
  (: NODE/k (-> Node Label Index (U A B C D)))
  (define (NODE/k node label label-offset)
    (if (= (label-length label) label-offset)
        (matched-at-node/k node)
        (let ([child (node-find-child node (label-ref label label-offset))])
          (if child
              (EDGE/k child label label-offset)
              (mismatched-at-node/k node label label-offset)))))
  (NODE/k node (label-copy original-label) 0))
