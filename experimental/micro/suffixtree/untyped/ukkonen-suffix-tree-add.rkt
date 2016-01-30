#lang racket/base

(provide tree-add!)
(provide suffix-tree-add!)

;; -----------------------------------------------------------------------------

(require "data-node.rkt"
         "data-label.rkt"
         "data-suffix-tree.rkt"
(only-in "label-make-label.rkt"
  make-label)
(only-in "label-label-length.rkt"
  label-length)
(only-in "label-sublabel.rkt"
  sublabel)
(only-in "structs-node-position-at-end.rkt"
  node-position-at-end?)
(only-in "structs-node-add-leaf.rkt"
  node-add-leaf!)
(only-in "structs-node-follow-k.rkt"
  node-follow/k)
(only-in "structs-node-up-splice-leaf.rkt"
  node-up-splice-leaf!)
(only-in "ukkonen-find-next-extension-point-add-suffix-link.rkt"
  find-next-extension-point/add-suffix-link!)
(only-in "ukkonen-try-to-set-suffix-edge.rkt"
  try-to-set-suffix-edge!)
(only-in "ukkonen-extend-at-point.rkt"
  extend-at-point!))

;; =============================================================================

;; suffix-tree-add!: tree label -> void
;; Adds a new label and its suffixes to the suffix tree.
;; Precondition: label is nonempty.
(define (suffix-tree-add! tree label)
  (define (do-construction! tree label)
    (define pr (add-first-suffix! tree label))
    (define starting-node (car pr))
    (define starting-offset (cdr pr))
    (add-rest-suffixes! label starting-node starting-offset))
  (define (add-first-suffix! tree label)
    (define (matched-at-node node)
      (report-implicit-tree-constructed))
    (define (matched-in-node node offset)
      (report-implicit-tree-constructed))
    (define (mismatched-at-node node label label-offset)
      (define leaf
        (node-add-leaf! node (sublabel label label-offset)))
      (cons node label-offset))
    (define (mismatched-in-node node offset label label-offset)
      (define-values (joint leaf)
        (node-up-splice-leaf! node offset (sublabel label label-offset)))
      (cons joint label-offset))
    (define res (node-follow/k
                 (suffix-tree-root tree)
                 label
                 matched-at-node
                 matched-in-node
                 mismatched-at-node
                 mismatched-in-node))
    ;(when (void? res) (error "foo"))
    res)
  (define (add-rest-suffixes! label starting-node starting-offset)
    (add-rest-suffixes-loop!
     label
     (let ([i (label-length label)]) (unless (integer? i) (error "ars")) i)
     (max starting-offset 1)
     1
     starting-node))
  (define (add-rest-suffixes-loop! label N i j active-node)
    (when (< j N)
      (define-values (next-extension-node next-extension-offset i*)
        (find-next-extension-point/add-suffix-link! active-node label i j))
      (if i*
          (begin
            (let ([new-active-node
                   (extend-at-point! (begin (unless next-extension-node (error "bar bar bar")) next-extension-node)
                                     (begin (unless next-extension-offset (error "fofodofdof")) next-extension-offset)
                                     label i*)])
                  (try-to-set-suffix-edge! active-node new-active-node)
                  (add-rest-suffixes-loop!
                   label
                   N
                   (let ([num (max i* (add1 j))]) (unless (integer? num) (error "foo")) num)
                   (let ([num (add1 j)]) (unless (integer? num) (error "foo")) num)
                   new-active-node)))
          (begin (report-implicit-tree-constructed)
                 (void)))))
  (define (report-implicit-tree-constructed)
    (define dummy-node (node (make-label "dummy") #f '() #f))
    (cons dummy-node 0))
  (do-construction! tree label))

(define tree-add! suffix-tree-add!)
