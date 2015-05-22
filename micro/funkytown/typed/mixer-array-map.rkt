#lang typed/racket/base

(provide array-map)

;; -----------------------------------------------------------------------------

(require "data-array-adapted.rkt"
         "type-aliases.rkt"
         (for-syntax racket/base)
         benchmark-util)

(require/typed/check "array-struct-array-default-strict.rkt"
  [array-default-strict! (-> Array Void)])
(require/typed/check "array-struct-unsafe-build-array.rkt"
  [unsafe-build-array (-> Indexes (-> Indexes Float) Array)])

(require/typed/check "array-broadcast-array-broadcast.rkt"
  [array-broadcast (-> Array Indexes Array)])
(require/typed/check "array-broadcast-array-shape-broadcast.rkt"
  [array-shape-broadcast (case-> ((Listof Indexes) -> Indexes)
                                 ((Listof Indexes) (U #f #t 'permissive) -> Indexes))])
(require/typed/check "array-broadcast-array-broadcasting.rkt"
  [array-broadcasting (Parameterof (U #f #t 'permissive))])

;; =============================================================================

(: array-map (case->
              (-> (-> Float Float Float) Array Array Array)
              (-> (-> Float Float) Array Array)))
(define array-map
  (case-lambda:
    [([f : (Float -> Float)] [arr : Array])
     (inline-array-map f arr)]
    [([f : (Float Float -> Float)] [arr0 : Array] [arr1 : Array])
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
         (define arr* (unsafe-build-array ds (λ ([js : Indexes]) (f (proc js)))))
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
             (define arr* (unsafe-build-array ds (λ ([js : Indexes]) (f (proc js) (procs js) ...))))
             (array-default-strict! arr*)
             arr*))))]))
