#lang racket/base

(provide tree-walk)

(require
  "structs.rkt"
  "label.rkt"
  "ukkonen2.rkt")

(define (tree-walk tree input-label succeed-f fail-f)
 (letrec [(comparing-label-elements
           (lambda (node up-label up-label-offset input-label-offset)
            (cond
             ((= input-label-offset (label-length input-label))
              (succeed-f node up-label-offset))
             ((= up-label-offset (label-length up-label))
              (choosing-next-edge node input-label-offset))
             ((label-element-equal?
               (label-ref up-label up-label-offset)
               (label-ref input-label input-label-offset))
              (comparing-label-elements node up-label
               (add1 up-label-offset)
               (add1 input-label-offset)))
             (else
              (fail-f node up-label-offset input-label-offset)))))
  (choosing-next-edge
   (lambda (node input-label-offset)
    (let ((child
           (node-find-child
            node (label-ref input-label input-label-offset))))
     (if child
      (comparing-label-elements child
       (node-up-label child) 0
       input-label-offset)
      (fail-f node (label-length (node-up-label node))
       input-label-offset)))))]
(if (= 0 (label-length input-label))
 (begin
  (succeed-f (tree-root tree) 0))
 ;; Start off the walk at the root.
 (choosing-next-edge (tree-root tree) 0))))
