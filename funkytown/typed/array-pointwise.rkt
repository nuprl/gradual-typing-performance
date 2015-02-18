#lang typed/racket/base

(provide array-map)

(require benchmark-util
         "../base/array-types.rkt"
         (for-syntax racket/base))

(require/typed/check "array-struct.rkt"
  [array? (-> Any Boolean)]
  [array-shape (-> (Array Any) Indexes)]
  [array-default-strict! (-> Indexes Void)]
  [unsafe-array-proc (-> (Array Any) (-> Indexes Any))]
  [unsafe-build-array (-> Indexes (-> Indexes Any) (Array Any))])

(require/typed/check "array-broadcast.rkt"
  [array-broadcast (-> (Array Any) Indexes (Array Any))]
  [array-shape-broadcast (case-> ((Listof Indexes) -> Indexes)
                                 ((Listof Indexes) (U #f #t 'permissive) -> Indexes))])

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
         (define arr* (unsafe-build-array ds (λ ([js : Indexes]) (f (proc js)))))
         (array-default-strict! arr*)
         arr*))]
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
             (define arr* (unsafe-build-array ds (λ ([js : Indexes]) (f (proc js) (procs js) ...))))
             (array-default-strict! arr*)
             arr*))))]))

(: array-map (All (R A B T ...)
                  (case->
                   (-> (-> A B R) (Array A) (Array B) (Array R))
                   (-> (-> A R) (Array A) (Array R)))))
(define array-map
  (case-lambda:
    [([f : (A -> R)] [arr : (Array A)])
     (inline-array-map f arr)]
    [([f : (A B -> R)] [arr0 : (Array A)] [arr1 : (Array B)])
     (inline-array-map f arr0 arr1)]))
