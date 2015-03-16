#lang racket/base
;; test infrastructure stuff.

(require rackunit
         rackunit/text-ui
         "structs.rkt"
         "label.rkt")

(provide (all-defined-out)
         (all-from-out rackunit)
         (all-from-out rackunit/text-ui)
         (all-from-out "structs.rkt")
         (all-from-out "label.rkt"))


;; set the error width printing wide
(error-print-width 800)

;; Quick and dirty code to get at a new root and tree.
(define (root-and-tree)
  (let* ((tree (new-suffix-tree))
         (root (suffix-tree-root tree)))
    (values root tree)))

;; Hand-coded partial construction of the suffix tree for string
;; "00000100$.  Meant to be used for testing phases of the Ukkonen
;; algorithm.
;;
;; Only two more leaves nead to be added to complete the tree: the
;; leaf with up-label "$" off of node 8, and the one with up-label $
;; from root0.
(define (tree-structure-for-00000100$)
  (let-values (((root0 tree) (root-and-tree)))
    (let* ((leaf1 (node-add-leaf! root0 (make-label "00000100$")))
           (node2 (node-up-split! leaf1 4))
           (node4 (node-up-split! node2 3))
           (node6 (node-up-split! node4 2))
           (node8 (node-up-split! node6 1))
           
           (leaf3 (node-add-leaf! node2 (make-label "100$")))
           (leaf5 (node-add-leaf! node4 (make-label "100$")))
           (leaf7 (node-add-leaf! node6 (make-label "100$")))
           (leaf9 (node-add-leaf! node8 (make-label "100$")))
           (leaf10 (node-add-leaf! root0 (make-label "100$"))))
      (set-node-suffix-link! node2 node4)
      (set-node-suffix-link! node4 node6)
      (set-node-suffix-link! node6 node8)
      (set-node-suffix-link! node8 root0)
      (values tree (list root0 leaf1 node2 leaf3 node4 leaf5
                         node6 leaf7 node8 leaf9 leaf10)))))



(define-simple-check (check-label-equal? label-1 label-2)
  (label-equal? label-1 label-2))


;; zfill: string number -> string
;; pads leading zeros so that string is of length number.
;; precondition: (<= (string-length s) n).
(define (zfill s n)
  (when (> (string-length s) n)
    (error 'zfill "~S's length already greater than ~A" s n))
  (string-append (make-string (- n (string-length s)) #\0) s))


(define (for-each-sublabel f label)
  (let loop ((i 0))
    (when (< i (label-length label))
      (f (sublabel label i))
      (loop (add1 i)))))

