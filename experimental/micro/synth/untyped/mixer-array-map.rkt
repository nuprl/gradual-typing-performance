#lang racket/base

(provide array-map)

;; -----------------------------------------------------------------------------

(require "data-array.rkt"
         (for-syntax racket/base)
(only-in "array-struct-array-default-strict.rkt" array-default-strict!)
(only-in "array-struct-unsafe-build-array.rkt" unsafe-build-array)
(only-in "array-broadcast-array-broadcast.rkt" array-broadcast)
(only-in "array-broadcast-array-shape-broadcast.rkt" array-shape-broadcast)
(only-in "array-broadcast-array-broadcasting.rkt" array-broadcasting))

;; =============================================================================

(define array-map
  (case-lambda
    [(f arr)
     (inline-array-map f arr)]
    [(f arr0 arr1)
     (inline-array-map f arr0 arr1)]))

;; -----------------------------------------------------------------------------

;; -- array-pointwise
(define-syntax-rule (ensure-array name arr-expr)
  (let ([arr arr-expr])
    (if (Array? arr) arr (raise-argument-error name "Array" arr))))

(define-syntax (inline-array-map stx)
  (syntax-case stx ()
    [(_ f arr-expr)
     (syntax/loc stx
       (let ([arr  (ensure-array 'array-map arr-expr)])
         (define ds (Array-shape arr))
         (define proc (Array-unsafe-proc arr))
         (define arr* (unsafe-build-array ds (λ (js) (f (proc js)))))
         (array-default-strict! arr*)
         arr*))]
    [(_ f arr-expr arr-exprs ...)
     (with-syntax ([(arrs ...)   (generate-temporaries #'(arr-exprs ...))]
                   [(procs ...)  (generate-temporaries #'(arr-exprs ...))])
       (syntax/loc stx
         (let ([arr   (ensure-array 'array-map arr-expr)]
               [arrs  (ensure-array 'array-map arr-exprs)] ...)
           (define ds (array-shape-broadcast (list (Array-shape arr) (Array-shape arrs) ...)))
           (let ([arr   (array-broadcast arr ds)]
                 [arrs  (array-broadcast arrs ds)] ...)
             (define proc  (Array-unsafe-proc arr))
             (define procs (Array-unsafe-proc arrs)) ...
             (define arr* (unsafe-build-array ds (λ (js) (f (proc js) (procs js) ...))))
             (array-default-strict! arr*)
             arr*))))]))
