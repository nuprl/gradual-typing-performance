#lang racket/base

(require (for-syntax racket/base syntax/parse)
         (only-in racket/unsafe/ops unsafe-fx+ unsafe-fx<)
         (only-in racket/fixnum fx+ fx*)
         (only-in "array-utils.rkt"
           array-shape-size
           check-array-shape-size
           check-array-shape
           next-indexes!
           unsafe-array-index->value-index)
         "../base/array-types.rkt")

(provide
 array?
 array-default-strict
 array-default-strict!
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
 unsafe-build-simple-array
 unsafe-vector->array)

;; -- array-for-each

(define-syntax-rule (for-each-array+data-index ds-expr f-expr)
  (let* ([ds ds-expr]
          [dims (vector-length ds)])
    (define-syntax-rule (f js j)
      (f-expr js j))
    (cond
      [(= dims 0)  (f ds 0)]
      [else
       (define js (make-vector dims 0))
       (case dims
         [(1)  (define d0 (vector-ref ds 0))
               (let j0-loop ([j0 0])
                 (when (j0 . < . d0)
                   (vector-set! js 0 j0)
                   (f js j0)
                   (j0-loop (+ j0 1))))]
         [(2)  (define d0 (vector-ref ds 0))
               (define d1 (vector-ref ds 1))
               (let j0-loop ([j0 0]
                                  [j 0])
                 (when (j0 . < . d0)
                   (vector-set! js 0 j0)
                   (let j1-loop ([j1 0]
                                     [j j])
                     (cond [(j1 . < . d1)
                            (vector-set! js 1 j1)
                            (f js j)
                            (j1-loop (+ j1 1) (fx+ j 1))]
                           [else
                            (j0-loop (+ j0 1) j)]))))]
         [else  (let i-loop ([i  0]
                                                   [j  0])
                  (cond [(i . < . dims)
                        (define di (vector-ref ds i))
                         (let ji-loop ([ji 0]
                                                             [j  j])
                           (cond [(ji . < . di)
                                  (vector-set! js i ji)
                                  (ji-loop (+ ji 1) (i-loop (+ i 1) j))]
                                 [else  j]))]
                        [else  (f js j)
                               (fx+ j 1)]))
                (void)])])))

(define-syntax-rule (for-each-array-index ds-expr f-expr)
  (let* ([ds  ds-expr]
          [dims   (vector-length ds)])
    (define-syntax-rule (f js)
      (f-expr js))
    (cond
      [(= dims 0)  (f ds)]
      [else
       (define js  (make-vector dims 0))
       (case dims
         [(1)  (define d0 (vector-ref ds 0))
               (let j0-loop ([j0   0])
                 (when (j0 . < . d0)
                   (vector-set! js 0 j0)
                   (f js)
                   (j0-loop (+ j0 1))))]
         [(2)  (define d0 (vector-ref ds 0))
               (define d1 (vector-ref ds 1))
               (let j0-loop ([j0  0])
                 (when (j0 . < . d0)
                   (vector-set! js 0 j0)
                   (let j1-loop  ([j1   0])
                     (cond [(j1 . < . d1)
                            (vector-set! js 1 j1)
                            (f js)
                            (j1-loop (+ j1 1))]
                           [else
                            (j0-loop (+ j0 1))]))))]
         [else  (let i-loop  ([i   0])
                  (cond [(i . < . dims)
                         (define di  (vector-ref ds i))
                         (let ji-loop  ([ji   0])
                           (when (ji . < . di)
                             (vector-set! js i ji)
                             (i-loop (+ i 1))
                             (ji-loop (+ ji 1))))]
                        [else  (f js)]))])])))

(define-syntax-rule (for-each-data-index ds-expr f-expr)
  (let* ([ds   ds-expr]
          [dims   (vector-length ds)])
    (define-syntax-rule (f j)
      (f-expr j))
    (cond
      [(= dims 0)  (f 0)]
      [else
       (case dims
         [(1)  (define d0  (vector-ref ds 0))
               (let j0-loop  ([j0   0])
                 (when (j0 . < . d0)
                   (f j0)
                   (j0-loop (+ j0 1))))]
         [(2)  (define d0  (vector-ref ds 0))
               (define d1  (vector-ref ds 1))
               (let j0-loop  ([j0   0]
                                     [j   0])
                 (when (j0 . < . d0)
                   (let j1-loop  ([j1   0]
                                         [j   j])
                     (cond [(j1 . < . d1)
                            (f j)
                            (j1-loop (+ j1 1) (fx+ j 1))]
                           [else
                            (j0-loop (+ j0 1) j)]))))]
         [else  (let i-loop  ([i   0]
                                                   [j   0])
                  (cond [(i . < . dims)
                         (define di  (vector-ref ds i))
                         (let ji-loop  ([ji   0]
                                                             [j   j])
                           (cond [(ji . < . di)
                                  (ji-loop (+ ji 1) (i-loop (+ i 1) j))]
                                 [else  j]))]
                        [else  (f j)
                               (fx+ j 1)]))
                (void)])])))

(define-syntax-rule (inline-build-array-data ds-expr g-expr A)
  (let* ([ds   ds-expr]
          [dims   (vector-length ds)])
    (define-syntax-rule (g js j)
      (g-expr js j))
    (define size 
      (let loop  ([k   0] [size   1])
        (cond [(k . < . dims)  (loop (+ k 1) (fx* size (vector-ref ds k)))]
              [else  size])))
    (cond [(= size 0)  (vector) ]
          [else
           (define js0  (make-vector dims 0))
           (define vs (make-vector size (g js0 0)))
           (for-each-array+data-index ds (λ (js j) (vector-set! vs j (g js j))))
           vs])))

;; -- array-struct

(define array-strictness (make-parameter #t))

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
