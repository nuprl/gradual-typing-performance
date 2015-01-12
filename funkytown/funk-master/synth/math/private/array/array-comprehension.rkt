#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         (only-in "mutable-array.rkt" unsafe-vector->array)
         (only-in "typed-utils.rkt" array-shape-size)
         (only-in typed/untyped-utils require/untyped-contract))

(require/untyped-contract
 "typed-utils.rkt"
 [check-array-shape  ((Vectorof Integer) (-> Nothing) -> (Vectorof Index))])

(provide for/array)

(define-syntax (base-for/array stx)
  (syntax-parse stx
    [(_ name:id for/vector:id #:shape ds-expr:expr (~optional (~seq #:fill fill-expr:expr))
        (clause ...) body:expr ...+)
     (with-syntax ([(maybe-fill ...)  (if (attribute fill-expr) #'(#:fill fill-expr) #'())])
       (syntax/loc stx
         (let* ([ds  ds-expr]
                [ds  (check-array-shape
                      ds (λ () (raise-argument-error 'name "Indexes" ds)))])
           (define vs (for/vector #:length (array-shape-size ds) maybe-fill ...
                        (clause ...) body ...))
           (unsafe-vector->array ds vs))))]
    [(_ name:id for/vector:id (clause ...) body:expr ...+)
     (syntax/loc stx
       (let ()
         (define vs (for/vector (clause ...) body ...))
         (define ds ((inst vector Index) (vector-length vs)))
         (unsafe-vector->array ds vs)))]))

(define-syntax-rule (for/array e ...)
  (base-for/array for/array for/vector e ...))
