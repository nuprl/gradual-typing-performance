#lang racket/base

(provide array-map)

(require (only-in "array-struct.rkt"
                    array?
                    array-shape
                    array-default-strict
                    unsafe-array-proc
                    unsafe-build-array)
           (only-in "array-broadcast.rkt" array-broadcast array-shape-broadcast)
           (for-syntax racket/base))

(define-syntax-rule (ensure-array name arr-expr)
  (let ([arr arr-expr])
    (if (array? arr) arr (raise-argument-error name "Array" arr))))

(define-syntax (inline-array-map stx)
  (syntax-case stx ()
    [(_ f arr-expr)
     (syntax/loc stx
       (let ([arr  (ensure-array 'array-map arr-expr)])
         (define ds (array-shape arr))
         (define proc (unsafe-array-proc arr))
         (array-default-strict
          (unsafe-build-array ds (λ (js) (f (proc js)))))))]
    [(_ f arr-expr arr-exprs ...)
     (with-syntax ([(arrs ...)   (generate-temporaries #'(arr-exprs ...))]
                   [(procs ...)  (generate-temporaries #'(arr-exprs ...))])
       (syntax/loc stx
         (let ([arr   (ensure-array 'array-map arr-expr)]
               [arrs  (ensure-array 'array-map arr-exprs)] ...)
           (define ds (array-shape-broadcast (list (array-shape arr) (array-shape arrs) ...)))
           (let ([arr   (array-broadcast arr ds)]
                 [arrs  (array-broadcast arrs ds)] ...)
             (define proc  (unsafe-array-proc arr))
             (define procs (unsafe-array-proc arrs)) ...
             (array-default-strict
              (unsafe-build-array ds (λ (js) (f (proc js) (procs js) ...))))))))]))

(define array-map
  (case-lambda
    [(f arr)  (inline-array-map f arr)]
    [(f arr0 arr1)  (inline-array-map f arr0 arr1)]))
