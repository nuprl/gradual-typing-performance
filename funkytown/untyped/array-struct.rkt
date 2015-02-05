#lang racket/base

(require (for-syntax racket/base syntax/parse)
         (only-in racket/unsafe/ops unsafe-fx+ unsafe-fx<)
         (only-in "array-for-each.rkt"
                  for-each-array-index
                  inline-build-array-data)
         (only-in "array-utils.rkt"
           array-shape-size
           check-array-shape-size
           check-array-shape
           next-indexes!
           unsafe-array-index->value-index))

(provide
 Array
 Settable-Array
 array?
 array-default-strict
 array-shape
 array-size
 array-strict?
 array-strictness
 build-array
 for/array
 in-array
 make-array
 make-unsafe-array-proc
 make-unsafe-array-set-proc
 unsafe-array-proc
 unsafe-build-array
 unsafe-build-simple-array)

(define array-strictness (make-parameter #t))

(struct Array ([shape ]
               [size ]
               [strict? ]
               [strict! ]
               [unsafe-proc ]))

(define-syntax-rule (make-unsafe-array-proc ds ref)
  (lambda (js)
    (ref (unsafe-array-index->value-index ds js))))

(define (array-strict? arr)
  (unbox (Array-strict? arr)))

(define (array-strict! arr)
  (define strict? (Array-strict? arr))
  (unless (unbox strict?)
    ((Array-strict! arr))
    (set-box! strict? #t)))

(define (array-default-strict! arr)
  (define strict? (Array-strict? arr))
  (when (and (not (unbox strict?)) (array-strictness))
    ((Array-strict! arr))
    (set-box! strict? #t)))

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
        (set-box! f (lambda (js )
                      (vector-ref vs (unsafe-array-index->value-index ds js))))))
    (define unsafe-proc
      (lambda (js) ((unbox f) js)))
    (Array ds size (box #f) strict! unsafe-proc)))

(define (unsafe-build-simple-array ds f)
  (define size (check-array-shape-size 'unsafe-build-simple-array ds))
  (Array ds size (box #t) void f))

(define (build-array ds proc)
  (let ([ds  (check-array-shape
              ds (lambda () (raise-argument-error 'build-array "(Vectorof Index)" 0 ds proc)))])
    (define arr
      (unsafe-build-array ds (lambda (js)
                               (proc (vector->immutable-vector js)))))
    (array-default-strict! arr)
    arr))

(define (build-simple-array ds proc)
  (let ([ds  (check-array-shape
              ds (lambda () (raise-argument-error 'build-simple-array "(Vectorof Index)" 0 ds proc)))])
    (unsafe-build-simple-array ds (lambda (js)
                                    (proc (vector->immutable-vector js))))))

;; ===================================================================================================
;; Abstract settable array data type

(struct Settable-Array Array ([set-proc ]))

(define settable-array? Settable-Array?)
(define unsafe-settable-array-set-proc Settable-Array-set-proc)

(define-syntax-rule (make-unsafe-array-set-proc A ds set!)
  (lambda (js v)
    (set! (unsafe-array-index->value-index ds js) v)))


;; -- from 'array-constructors.rkt'

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
             (let ([arr  arr-expr])
               (cond [(array? arr)
                      (define ds (array-shape arr))
                      (define dims (vector-length ds))
                      (define size (array-size arr))
                      (define proc (unsafe-array-proc arr))
                      (define js (make-vector dims 0))
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

(struct Mutable-Array Settable-Array ([data ]))

(define (unsafe-vector->array ds vs)
  (define proc (make-unsafe-array-proc ds (λ (j) (vector-ref vs j))))
  (define set-proc (make-unsafe-array-set-proc A ds (λ (j v) (vector-set! vs j v))))
  (Mutable-Array ds (vector-length vs) (box #t) void proc set-proc vs))

;; -- from array-comprehension

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
         (define ds (vector (vector-length vs)))
         (unsafe-vector->array ds vs)))]))

(define-syntax-rule (for/array e ...)
  (base-for/array for/array for/vector e ...))
