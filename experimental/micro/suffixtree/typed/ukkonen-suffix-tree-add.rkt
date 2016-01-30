#lang typed/racket/base

(provide tree-add!)
(provide suffix-tree-add!)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt"
         "data-label-adapted.rkt"
         "data-suffix-tree-adapted.rkt"
         benchmark-util)
(require/typed/check "label-make-label.rkt"
  [make-label (-> (U String (Vectorof (U Char Symbol))) label)])
(require/typed/check "label-label-length.rkt"
  [label-length (-> label Index)])
(require/typed/check "label-sublabel.rkt"
  [sublabel (-> label Index label)])
(require/typed/check "structs-node-position-at-end.rkt"
  [node-position-at-end? (-> Node Index Boolean)])
(require/typed/check "structs-node-add-leaf.rkt"
  [node-add-leaf! (-> Node Label Node)])
(require/typed/check "structs-node-follow-k.rkt"
  [node-follow/k (-> Node
                     Label
                     (-> Node (Pairof Node Index))
                     (-> Node Index (Pairof Node Index))
                     (-> Node Label Index (Pairof Node Index))
                     (-> Node Index Label Index (Pairof Node Index))
                     (Pairof Node Index))])
(require/typed/check "structs-node-up-splice-leaf.rkt"
  [node-up-splice-leaf! (-> Node Index Label (values Node Node))])
(require/typed/check "ukkonen-find-next-extension-point-add-suffix-link.rkt"
  [find-next-extension-point/add-suffix-link! (-> Node Label Index Index (values (U #f Node) (U #f Index) (U #f Index)))])
(require/typed/check "ukkonen-try-to-set-suffix-edge.rkt"
  [try-to-set-suffix-edge! (-> Node Node Void)])
(require/typed/check "ukkonen-extend-at-point.rkt"
  [extend-at-point! (-> Node Index Label Index Node)])

;; =============================================================================

;; suffix-tree-add!: tree label -> void
;; Adds a new label and its suffixes to the suffix tree.
;; Precondition: label is nonempty.
(: suffix-tree-add! (-> Tree Label Void))
(define (suffix-tree-add! tree label)
  (: do-construction! (-> Tree Label Void))
  (define (do-construction! tree label)
    (define pr (add-first-suffix! tree label))
    (define starting-node (car pr))
    (define starting-offset (cdr pr))
    (add-rest-suffixes! label starting-node starting-offset))
  (: add-first-suffix! (-> Tree Label (Pairof Node Index)))
  (define (add-first-suffix! tree label)
    (: matched-at-node (-> Node (Pairof Node Index)))
    (define (matched-at-node node)
      (report-implicit-tree-constructed))
    (: matched-in-node (-> Node Index (Pairof Node Index)))
    (define (matched-in-node node offset)
      (report-implicit-tree-constructed))
    (: mismatched-at-node (-> Node Label Index (Pairof Node Index)))
    (define (mismatched-at-node node label label-offset)
      (define leaf
        (node-add-leaf! node (sublabel label label-offset)))
      (cons node label-offset))
    (: mismatched-in-node (-> Node Index Label Index (Pairof Node Index)))
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
  (: add-rest-suffixes! (-> Label Node Index Void))
  (define (add-rest-suffixes! label starting-node starting-offset)
    (add-rest-suffixes-loop!
     label
     (let ([i (label-length label)]) (unless (index? i) (error "ars")) i)
     (max starting-offset 1)
     1
     starting-node))
  (: add-rest-suffixes-loop! (-> Label Index Index Index Node Void))
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
                   (let ([num (max i* (add1 j))]) (unless (index? num) (error "foo")) num)
                   (let ([num (add1 j)]) (unless (index? num) (error "foo")) num)
                   new-active-node)))
          (begin (report-implicit-tree-constructed)
                 (void)))))
  (: report-implicit-tree-constructed (-> (Pairof Node Index)))
  (define (report-implicit-tree-constructed)
    (define dummy-node (node (make-label "dummy") #f '() #f))
    (cons dummy-node 0))
  (do-construction! tree label))

(define tree-add! suffix-tree-add!)
