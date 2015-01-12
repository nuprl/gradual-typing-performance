#lang typed/racket/base

(provide inline-array-map)

(module syntax-defs typed/racket/base
  (require (for-syntax typed/racket/base)
           (only-in "array-struct.rkt"
                    Array
                    Array?
                    Array-shape
                    array-default-strict
                    Array-unsafe-proc
                    unsafe-build-array)
           (only-in "array-broadcast.rkt" array-broadcast array-shape-broadcast)
           (only-in "utils.rkt" Indexes))
  
  (provide inline-array-map)
  
  (define-syntax-rule (ensure-array name arr-expr)
    (plet: (A) ([arr : (Array A)  arr-expr])
      (if (Array? arr) arr (raise-argument-error name "Array" arr))))
  
  (define-syntax (inline-array-map stx)
    (syntax-case stx ()
      [(_ f)
       (syntax/loc stx
         (array-default-strict
          (unsafe-build-array #() (λ (js) (f)))))]
      [(_ f arr-expr)
       (syntax/loc stx
         (let ([arr  (ensure-array 'array-map arr-expr)])
           (define ds (Array-shape arr))
           (define proc (Array-unsafe-proc arr))
           (array-default-strict
            (unsafe-build-array ds (λ: ([js : Indexes]) (f (proc js)))))))]
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
               (array-default-strict
                (unsafe-build-array ds (λ: ([js : Indexes]) (f (proc js) (procs js) ...))))))))])))

(require 'syntax-defs)
