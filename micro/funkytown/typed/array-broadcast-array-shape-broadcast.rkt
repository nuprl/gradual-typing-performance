#lang typed/racket/base

(provide array-shape-broadcast)

;; -----------------------------------------------------------------------------

(require "type-aliases.rkt"
         (only-in racket/string string-join)
         benchmark-util)
(require/typed/check "array-broadcast-array-broadcasting.rkt"
  [array-broadcasting (Parameterof (U #f #t 'permissive))])
(require/typed/check "array-broadcast-shape-broadcast2.rkt"
  [shape-broadcast2 (Indexes Indexes (-> Nothing) (U #f #t 'permissive) -> Indexes)])

;; =============================================================================

(: array-shape-broadcast (case-> ((Listof Indexes) -> Indexes)
                                 ((Listof Indexes) (U #f #t 'permissive) -> Indexes)))
(define (array-shape-broadcast dss [broadcasting (array-broadcasting)])
  (define (fail) (error 'array-shape-broadcast
                        "incompatible array shapes (array-broadcasting ~v): ~a"
                        broadcasting
                        (string-join (map (λ (ds) (format "~e" ds)) dss) ", ")))
  (cond [(eq? '() dss)  #()]
        [else  (for/fold ([new-ds  (car dss)]) ([ds  (in-list (cdr dss))])
                 (shape-broadcast2 new-ds ds fail broadcasting))]))
