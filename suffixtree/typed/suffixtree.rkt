#lang typed/racket/base

(provide tree-walk)

(require
  "structs.rkt"
  "label.rkt"
  "ukkonen2.rkt")

(: tree-walk (All (A) (-> Tree Label (-> Node Index A) (-> Node Index Index A) A)))
(define (tree-walk tree input-label succeed-f fail-f)
  (: comparing-label-elements (-> Node Label Index Index A))
  (define (comparing-label-elements node up-label up-label-offset input-label-offset)
    (cond [(= input-label-offset (label-length input-label))
           (succeed-f node up-label-offset)]
          [(= up-label-offset (label-length up-label))
           (choosing-next-edge node input-label-offset)]
          [(label-element-equal?
            (label-ref up-label up-label-offset)
            (label-ref input-label input-label-offset))
           (let ([a (add1 up-label-offset)]
                 [b (add1 input-label-offset)])
             (unless (and (index? a) (index? b)) (error "tree-walk"))
             (comparing-label-elements node up-label a b))]
          [else
            (fail-f node up-label-offset input-label-offset)]))
  (: choosing-next-edge (-> Node Index A))
  (define (choosing-next-edge node input-label-offset)
    (define child (node-find-child node (label-ref input-label input-label-offset)))
    (define i (label-length (node-up-label node)))
    (cond [child
           (comparing-label-elements child (node-up-label child) 0 input-label-offset)]
          [(index? i)
           (fail-f node i input-label-offset)]
          [else (error "tree-walk")]))
  (cond [(= 0 (label-length input-label))
         (succeed-f (tree-root tree) 0)]
        [else
         (choosing-next-edge (tree-root tree) 0)]))
