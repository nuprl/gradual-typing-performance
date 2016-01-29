#lang racket/base

(provide (struct-out node))

;; -----------------------------------------------------------------------------

(require "data-label.rkt")

;; =============================================================================

;; up-label: label
;; parent: (union #f node)
;; children: (listof node)
;; suffix-link: (union #f node)
(struct node
  (up-label
   parent
   children
   suffix-link)
  #:mutable)
