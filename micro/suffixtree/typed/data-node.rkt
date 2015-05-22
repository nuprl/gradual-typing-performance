#lang typed/racket/base

(provide (struct-out node))

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt")

;; =============================================================================

;; up-label: label
;; parent: (union #f node)
;; children: (listof node)
;; suffix-link: (union #f node)
(struct node
  ([up-label : label]
   [parent : (U #f node)]
   [children : (Listof node)]
   [suffix-link : (U #f node)])
  #:mutable)
