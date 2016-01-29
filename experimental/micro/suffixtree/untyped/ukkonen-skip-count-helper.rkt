#lang racket/base

(provide skip-count-helper)

;; -----------------------------------------------------------------------------

(require "data-node.rkt"
         "data-label.rkt"
(only-in "label-label-length.rkt"
  label-length)
(only-in "label-label-ref.rkt"
  label-ref)
(only-in "structs-node-find-child.rkt"
  node-find-child))

;; =============================================================================

;; Utility function for skip count, but also visible for those in
;; the know to skip-count from an arbitrary position in label.
(define (skip-count-helper node label k N)
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
          (begin (unless (integer? rest-of-chars-left-to-skip) (error "skip-count=hlper !!!"))
                 (values child rest-of-chars-left-to-skip)))))
  (if (>= k N)
      (values node (label-length (node-up-label node)))
      (loop node k)))
