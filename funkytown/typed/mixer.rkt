#lang typed/racket/base

(require benchmark-util
         "../base/array-types.rkt"
         (for-syntax racket/base)
         (only-in racket/list first second rest))

(require/typed/check "array-struct.rkt"
  [array? (-> (Array Any) Boolean)]
  [array-shape (-> (Array Float) Indexes)]
  [array-default-strict! (-> (Array Float) Void)]
  [unsafe-array-proc (-> (Array Float) (-> Indexes Float))]
  [unsafe-build-array (-> Indexes (-> Indexes Float) (Array Float))])

(require/typed/check "array-broadcast.rkt"
  [array-broadcast (-> (Array Float) Indexes (Array Float))]
  [array-shape-broadcast (case-> ((Listof Indexes) -> Indexes)
                                 ((Listof Indexes) (U #f #t 'permissive) -> Indexes))]
  [array-broadcasting (Parameterof (U #f #t 'permissive))])

(provide mix)

;; -- array-pointwise
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

(: array-map
                  (case->
                   (-> (-> Float Float Float) (Array Float) (Array Float) (Array Float))
                   (-> (-> Float Float) (Array Float) (Array Float))))
(define array-map
  (case-lambda:
    [([f : (Float -> Float)] [arr : (Array Float)])
     (inline-array-map f arr)]
    [([f : (Float Float -> Float)] [arr0 : (Array Float)] [arr1 : (Array Float)])
     (inline-array-map f arr0 arr1)]))

;; Weighted sum of signals, receives a list of lists (signal weight).
;; Shorter signals are repeated to match the length of the longest.
;; Normalizes output to be within [-1,1].

(: mix (-> Weighted-Signal * (Array Float)))
(define (mix . ss)
  (: signals (Listof (Array Float)))
  (define signals
    (for/list : (Listof (Array Float)) ([s : Weighted-Signal ss])
      (first s)))
  (: weights (Listof Float))
  (define weights
    (for/list : (Listof Float) ([x : Weighted-Signal ss])
      (real->double-flonum (second x))))
  (: downscale-ratio Float)
  (define downscale-ratio (/ 1.0 (apply + weights)))
  (: scale-signal (Float -> (Float -> Float)))
  (define ((scale-signal w) x) (* x w downscale-ratio))
  (parameterize ([array-broadcasting 'permissive]) ; repeat short signals
    (for/fold ([res : (Array Float) (array-map (scale-signal (first weights))
                               (first signals))])
        ([s (in-list (rest signals))]
         [w (in-list (rest weights))])
      (define scale (scale-signal w))
      (array-map (lambda ([acc : Float]
                          [new : Float])
                   (+ acc (scale new)))
                 res s))))
