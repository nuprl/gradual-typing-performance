#lang typed/racket/base
(provide
  CCTable
  Cell%
  Door%
  Pos
  Grid
)
(require
  typed/racket/class
)

;; =============================================================================

;; TODO can I use a supertype? Getting errors with init-fields
(define-type CCTable (HashTable Char (U Door% Cell%)))

(define-type Cell%
  (Class
    (init-field
     (items (Listof Any) #:optional)
     (occupant (U #f (Instance Cell%)) #:optional))
    (free? (-> Boolean))
    (open (-> Void))
    (show (-> Char))
    (close (-> Void))))

(define-type Door% Cell%)
;  (Class
;   #:implements/inits Cell%
;   (init-field
;     (open? Boolean #:optional))))

(define-type Pos (Vector Index Index))

(define-type Grid (Vectorof (Vectorof (Instance Cell%))))
;(define-type Grid (Mutable-Array (Instance Cell%)))
