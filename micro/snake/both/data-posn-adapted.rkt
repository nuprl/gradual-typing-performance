#lang typed/racket/base

(provide Posn
         NEList
         (struct-out posn))

;; -----------------------------------------------------------------------------

(require benchmark-util)

;; =============================================================================

(define-type Posn  posn)

(define-type NEList (Pairof Posn (Listof Posn)))

(require/typed/check "data-posn.rkt"
  [#:struct posn
            ([x : Real]
             [y : Real])])
