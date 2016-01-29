#lang racket/base

(provide longest-common-sublabel)

;; -----------------------------------------------------------------------------

(require "data-node.rkt"
         "data-label.rkt"
         "data-suffix-tree.rkt"
(only-in "label-make-label.rkt" make-label)
(only-in "label-label-source-eq.rkt" label-source-eq?)
(only-in "label-label-length.rkt" label-length)
(only-in "label-string-label.rkt" string->label)
(only-in "structs-new-suffix-tree.rkt" new-suffix-tree)
(only-in "ukkonen-suffix-tree-add.rkt" tree-add!)
(only-in "lcs-path-label.rkt" path-label))

;; =============================================================================

;; longest-common-sublabel: label label -> label
;;
;; Naive use of suffix trees to find longest common sublabel between
;; two labels.  Note that there's a better way to do this with
;; matching statistics: I'll try using matching statistics as soon
;; as I get this version running.
;;
;; This approach simply adds both labels to a common suffix tree,
;; does a postorder traversal to mark up the inner nodes, and then
;; finds the inner node with the deepest string depth.
(define (longest-common-sublabel label-1 label-2)
  (define label-1-marks (make-hasheq))
  (define label-2-marks (make-hasheq))
  (define deepest-node (node (make-label "no lcs") #f '() #f))
  (define deepest-depth 0)
  (define (main)
    (define tree (new-suffix-tree))
    (tree-add! tree label-1)
    (tree-add! tree label-2)
    (mark-up-inner-nodes! (suffix-tree-root tree) 0)
    (path-label deepest-node))
  (define (mark-up-inner-nodes! node depth)
    (cond [(null? (node-children node))
           (when (label-source-eq? (node-up-label node) label-1)
             (mark-with-label-1! node))
           (when (label-source-eq? (node-up-label node) label-2)
             (mark-with-label-2! node))]
          [else
            (for ([child (node-children node)])
              (let ([i (+ depth (label-length (node-up-label child)))])
                (unless (integer? i) (error "NOOOOO"))
                (mark-up-inner-nodes! child i)))
            (absorb-children-marks! node depth)]))
  (define (mark-with-label-1! node)
    (hash-set! label-1-marks node #t))
  (define (mark-with-label-2! node)
    (hash-set! label-2-marks node #t))
  (define (marked-by-label-1? node)
    (hash-ref label-1-marks node false-thunk))
  (define (marked-by-label-2? node)
    (hash-ref label-2-marks node false-thunk))
  (define (marked-by-both? node)
    (and (marked-by-label-1? node)
         (marked-by-label-2? node)))
  (define (absorb-children-marks! node depth)
    ;(let/ec escape
      (for ([child (node-children node)])
        (when (marked-by-label-1? child)
          (mark-with-label-1! node))
        (when (marked-by-label-2? child)
          (mark-with-label-2! node)))
        ;(when (marked-by-both? node)
        ;  (escape))))
    (when (and (marked-by-both? node)
               (> depth deepest-depth))
      (set! deepest-depth depth)
      (set! deepest-node node)))
  (if (or (= 0 (label-length label-1))
          (= 0 (label-length label-2)))
      (string->label "")
      (main)))

;; -----------------------------------------------------------------------------

(define false-thunk (lambda () #f))
