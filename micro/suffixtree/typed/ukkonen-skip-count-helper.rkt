#lang typed/racket/base

(provide skip-count-helper)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt"
         "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-label-length.rkt"
  [label-length (-> label Index)])
(require/typed/check "label-label-ref.rkt"
  [label-ref (-> label Integer (U Char Symbol))])
(require/typed/check "structs-node-find-child.rkt"
  [node-find-child (-> Node Any (U Node #f))])

;; =============================================================================

;; Utility function for skip count, but also visible for those in
;; the know to skip-count from an arbitrary position in label.
(: skip-count-helper (-> Node Label Index Index (values Node Index)))
(define (skip-count-helper node label k N)
  (: loop (-> Node Integer (values Node Index)))
  (define (loop node k)
    (let* ((child (node-find-child node (label-ref label k)))
           (child-label (begin
                          (unless child (error "skip-count-hlper"))
                          (node-up-label child)))
           (child-label-length (label-length child-label))
           (rest-of-chars-left-to-skip (- N k)))
      (if (> rest-of-chars-left-to-skip child-label-length)
          (loop child
                (+ k child-label-length))
          (begin (unless (index? rest-of-chars-left-to-skip) (error "skip-count=hlper !!!"))
                 (values child rest-of-chars-left-to-skip)))))
  (if (>= k N)
      (values node (label-length (node-up-label node)))
      (loop node k)))
