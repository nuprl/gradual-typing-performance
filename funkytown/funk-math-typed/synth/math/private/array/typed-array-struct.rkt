#lang typed/racket/base

(require racket/promise
         (only-in "../unsafe.rkt" unsafe-vector-ref)
         (only-in "for-each.rkt"
                  for-each-array-index
                  inline-build-array-data)
         "utils.rkt")

(provide (all-defined-out))

(: array-strictness (Parameterof (U #f #t)))
(define array-strictness (make-parameter #t))

;; ===================================================================================================
;; Array data type: a function whose domain has a rectangular shape

#|
(: array-procedure (All (A) ((Array A) In-Indexes -> A)))
(define (array-procedure arr js)
  ((Array-unsafe-proc arr) (check-array-indexes 'array-ref (Array-shape arr) js)))
|#

(struct: (A) Array ([shape : Indexes]
                    [size : Index]
                    [strict? : (Boxof Boolean)]
                    [strict! : (-> Void)]
                    [unsafe-proc : (Indexes -> A)]))

(define-syntax-rule (make-unsafe-array-proc ds ref)
  (λ: ([js : Indexes])
    (ref (unsafe-array-index->value-index ds js))))

(: array-strict? (All (A) ((Array A) -> Boolean)))
(define (array-strict? arr)
  (unbox (Array-strict? arr)))

(: array-strict! (All (A) ((Array A) -> Void)))
(define (array-strict! arr)
  (define strict? (Array-strict? arr))
  (unless (unbox strict?)
    ((Array-strict! arr))
    (set-box! strict? #t)))

(: array-default-strict! (All (A) ((Array A) -> Void)))
(define (array-default-strict! arr)
  (define strict? (Array-strict? arr))
  (when (and (not (unbox strict?)) (array-strictness))
    ((Array-strict! arr))
    (set-box! strict? #t)))

(: unsafe-build-array (All (A) (Indexes (Indexes -> A) -> (Array A))))
(define (unsafe-build-array ds f)
  ;; This box's contents get replaced when the array we're constructing is made strict, so that
  ;; the array stops referencing f. If we didn't do this, long chains of array computations would
  ;; keep hold of references to all the intermediate procs, which is a memory leak.
  (let ([f  (box f)])
    (define size (check-array-shape-size 'unsafe-build-array ds))
    ;; Sharp readers might notice that strict! doesn't check to see whether the array is already
    ;; strict; that's okay - array-strict! does it instead, which makes the "once strict, always
    ;; strict" invariant easier to ensure in subtypes, which we don't always have control over
    (define (strict!)
      (let* ([old-f  (unbox f)]
             [vs     (inline-build-array-data ds (λ (js j) (old-f js)) A)])
        ;; Make a new f that just indexes into vs
        (set-box! f (λ: ([js : Indexes])
                      (unsafe-vector-ref vs (unsafe-array-index->value-index ds js))))))
    (define unsafe-proc
      (λ: ([js : Indexes]) ((unbox f) js)))
    (Array ds size ((inst box Boolean) #f) strict! unsafe-proc)))

(: unsafe-build-simple-array (All (A) (Indexes (Indexes -> A) -> (Array A))))
(define (unsafe-build-simple-array ds f)
  (define size (check-array-shape-size 'unsafe-build-simple-array ds))
  (Array ds size (box #t) void f))

(: build-array (All (A) (In-Indexes (Indexes -> A) -> (Array A))))
(define (build-array ds proc)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'build-array "(Vectorof Index)" 0 ds proc)))])
    (define arr
      (unsafe-build-array ds (λ: ([js : Indexes])
                               (proc (vector->immutable-vector js)))))
    (array-default-strict! arr)
    arr))

(: build-simple-array (All (A) (In-Indexes (Indexes -> A) -> (Array A))))
(define (build-simple-array ds proc)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'build-simple-array "(Vectorof Index)" 0 ds proc)))])
    (unsafe-build-simple-array ds (λ: ([js : Indexes])
                                    (proc (vector->immutable-vector js))))))

;; ===================================================================================================
;; Abstract settable array data type

(struct: (A) Settable-Array Array ([set-proc : (Indexes A -> Void)]))

(define settable-array? Settable-Array?)
(define unsafe-settable-array-set-proc Settable-Array-set-proc)

(define-syntax-rule (make-unsafe-array-set-proc A ds set!)
  (λ: ([js : Indexes] [v : A])
    (set! (unsafe-array-index->value-index ds js) v)))

