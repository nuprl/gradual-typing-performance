#lang typed/racket/base

(provide jump-to-suffix)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt"
         "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-label-length.rkt"
  [label-length (-> label Index)])
(require/typed/check "structs-node-root.rkt"
  [node-root? (-> Node Boolean)])

;; =============================================================================

;; jump-to-suffix: node -> (values node (union boolean number))
;;
;; Given an internal node, jumps to the suffix from that node.
;; According to the theory of suffix trees, such a node will exist
;; in the tree if we follow the Ukkonen construction.  If we had to
;; go up a few characters, returns the number of chars at the suffix
;; end that need to be compared to get the real suffix.

;; If we hit the root, that offset is #f to indicate that we have to
;; start searching the suffix from scratch.
(: jump-to-suffix (-> Node (values Node (U Boolean Integer))))
(define (jump-to-suffix node)
  (define PARENT (node-parent node))
  (cond ((node-root? node)
         (values node #f))
        ((node-suffix-link node)
         (begin
                (let ([node2 (node-suffix-link node)])
                  (unless node2 (error "jump to suffix"))
                  (values node2 0))))
        ((and PARENT (node-root? PARENT))
           (values PARENT #f))
        (else
         (let* ([parent (node-parent node)]
                [sl (begin (unless parent (error "j2s"))
                           (node-suffix-link parent))])
           (unless sl (error "j2s whoahao"))
           (values sl
                   (label-length (node-up-label node)))))))
