#lang typed/racket/base

(require (for-syntax racket/base syntax/parse)
         (only-in racket/unsafe/ops unsafe-fx+ unsafe-fx<)
         benchmark-util
         "array-types.rkt")

(require/typed/check "array-utils.rkt"
)

(require/typed/check "array-for-each.rkt"
  [inline-build-array-data (-> Indexes (-> Indexes Any Any) (Array Any))])

(provide
 array?
 array-default-strict
 array-shape
 array-size
 array-strict?
 array-strictness
 build-array
 in-array
 make-array
 make-unsafe-array-proc
 make-unsafe-array-set-proc
 unsafe-array-proc
 unsafe-build-array
 unsafe-build-simple-array
 unsafe-vector->array
 )

(: array-strictness (Parameterof (U #f #t)))
(define array-strictness (make-parameter #t))

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
             [vs     (inline-build-array-data ds (lambda (js j) (old-f js)) A)])
        ;; Make a new f that just indexes into vs
        (set-box! f (λ: ([js : Indexes])
                      (vector-ref vs (unsafe-array-index->value-index ds js))))))
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
              ds (lambda () (raise-argument-error 'build-array "(Vectorof Index)" 0 ds proc)))])
    (define arr
      (unsafe-build-array ds (λ: ([js : Indexes])
                               (proc (vector->immutable-vector js)))))
    (array-default-strict! arr)
    arr))

(: build-simple-array (All (A) (In-Indexes (Indexes -> A) -> (Array A))))
(define (build-simple-array ds proc)
  (let ([ds  (check-array-shape
              ds (lambda () (raise-argument-error 'build-simple-array "(Vectorof Index)" 0 ds proc)))])
    (unsafe-build-simple-array ds (λ: ([js : Indexes])
                                    (proc (vector->immutable-vector js))))))

;; ===================================================================================================
;; Abstract settable array data type

(define settable-array? Settable-Array?)
(define unsafe-settable-array-set-proc Settable-Array-set-proc)

(define-syntax-rule (make-unsafe-array-set-proc A ds set!)
  (λ: ([js : Indexes] [v : A])
    (set! (unsafe-array-index->value-index ds js) v)))

;; -- from 'array-constructors.rkt'

(: make-array (All (A) (In-Indexes A -> (Array A))))
(define (make-array ds v)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'make-array "(Vectorof Index)" 0 ds v)))])
    (unsafe-build-simple-array ds (λ (js) v))))

;; --- Syntax

(define-syntax array? (make-rename-transformer #'Array?))
(define-syntax array-shape (make-rename-transformer #'Array-shape))
(define-syntax array-size (make-rename-transformer #'Array-size))
(define-syntax unsafe-array-proc (make-rename-transformer #'Array-unsafe-proc))

(define-syntax-rule (array-strict arr-expr)
  (let ([arr arr-expr])
    (array-strict! arr)
    arr))

(define-syntax-rule (array-default-strict arr-expr)
  (let ([arr arr-expr])
    (array-default-strict! arr)
    arr))

;; --- from array-sequence.rkt

(define-sequence-syntax in-array
  (λ () #'in-array)
  (λ (stx)
    (syntax-case stx ()
      [[(x) (_ arr-expr)]
       (syntax/loc stx
         [(x)
          (:do-in
           ([(ds size dims js proc)
             (plet: (A) ([arr : (Array A)  arr-expr])
               (cond [(array? arr)
                      (define ds (array-shape arr))
                      (define dims (vector-length ds))
                      (define size (array-size arr))
                      (define proc (unsafe-array-proc arr))
                      (define: js : Indexes (make-vector dims 0))
                      (values ds size dims js proc)]
                     [else
                      (raise-argument-error 'in-array "Array" arr)]))])
           (void)
           ([j 0])
           (unsafe-fx< j size)
           ([(x)  (proc js)])
           #true
           #true
           [(begin (next-indexes! ds dims js)
                   (unsafe-fx+ j 1))])])]
      [[_ clause] (raise-syntax-error 'in-array "expected (in-array <Array>)" #'clause #'clause)])))

;; --- from mutable-array.rkt

(: unsafe-vector->array (All (A) (Indexes (Vectorof A) -> (Mutable-Array A))))
(define (unsafe-vector->array ds vs)
  (define proc (make-unsafe-array-proc ds (λ (j) (vector-ref vs j))))
  (define set-proc (make-unsafe-array-set-proc A ds (λ (j v) (vector-set! vs j v))))
  (Mutable-Array ds (vector-length vs) (box #t) void proc set-proc vs))
