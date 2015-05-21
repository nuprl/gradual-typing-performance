#lang typed/racket/base

(provide longest-common-sublabel)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt"
         "data-label-adapted.rkt"
         "data-suffix-tree-adapted.rkt"
         benchmark-util)
(require/typed/check "label-make-label.rkt"
  [make-label (-> (U String (Vectorof (U Char Symbol))) label)])
(require/typed/check "label-label-source-eq.rkt"
  [label-source-eq? (-> label label Boolean)])
(require/typed/check "label-label-length.rkt"
  [label-length (-> label Index)])
(require/typed/check "label-string-label.rkt"
  [string->label (-> String label)])
(require/typed/check "structs-new-suffix-tree.rkt"
  [new-suffix-tree (-> Tree)])
(require/typed/check "ukkonen-suffix-tree-add.rkt"
  [tree-add! (-> Tree Label Void)])
(require/typed/check "lcs-path-label.rkt"
  [path-label (-> Node Label)])

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
(: longest-common-sublabel (-> Label Label Label))
(define (longest-common-sublabel label-1 label-2)
  (: label-1-marks (HashTable Node Boolean))
  (define label-1-marks (make-hasheq))
  (: label-2-marks (HashTable Node Boolean))
  (define label-2-marks (make-hasheq))
  (: deepest-node Node)
  (define deepest-node (node (make-label "no lcs") #f '() #f))
  (: deepest-depth Index)
  (define deepest-depth 0)
  (: main (-> Label))
  (define (main)
    (define tree (new-suffix-tree))
    (tree-add! tree label-1)
    (tree-add! tree label-2)
    (mark-up-inner-nodes! (suffix-tree-root tree) 0)
    (path-label deepest-node))
  (: mark-up-inner-nodes! (-> Node Index Void))
  (define (mark-up-inner-nodes! node depth)
    (cond [(null? (node-children node))
           (when (label-source-eq? (node-up-label node) label-1)
             (mark-with-label-1! node))
           (when (label-source-eq? (node-up-label node) label-2)
             (mark-with-label-2! node))]
          [else
            (for ([child (node-children node)])
              (let ([i (+ depth (label-length (node-up-label child)))])
                (unless (index? i) (error "NOOOOO"))
                (mark-up-inner-nodes! child i)))
            (absorb-children-marks! node depth)]))
  (: mark-with-label-1! (-> Node Void))
  (define (mark-with-label-1! node)
    (hash-set! label-1-marks node #t))
  (: mark-with-label-2! (-> Node Void))
  (define (mark-with-label-2! node)
    (hash-set! label-2-marks node #t))
  (: marked-by-label-1? (-> Node Boolean))
  (define (marked-by-label-1? node)
    (hash-ref label-1-marks node false-thunk))
  (: marked-by-label-2? (-> Node Boolean))
  (define (marked-by-label-2? node)
    (hash-ref label-2-marks node false-thunk))
  (: marked-by-both? (-> Node Boolean))
  (define (marked-by-both? node)
    (and (marked-by-label-1? node)
         (marked-by-label-2? node)))
  (: absorb-children-marks! (-> Node Index Void))
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

(: false-thunk (-> #f))
(define false-thunk (lambda () #f))
