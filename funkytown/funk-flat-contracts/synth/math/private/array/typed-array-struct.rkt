#lang racket/base

(require racket/promise
         (only-in "../unsafe.rkt" unsafe-vector-ref)
         (only-in "for-each.rkt"
                  for-each-array-index
                  inline-build-array-data)
         "utils.rkt"
         racket/contract)

(provide (all-defined-out))

(define array-strictness (make-parameter #t))

;; ===================================================================================================
;; Array data type: a function whose domain has a rectangular shape

#|
(define (array-procedure arr js)
  ((Array-unsafe-proc arr) (check-array-indexes 'array-ref (Array-shape arr) js)))
|#

(struct Array ([shape ]
                    [size ]
                    [strict? ]
                    [strict! ]
                    [unsafe-proc ]))

(define-syntax-rule (make-unsafe-array-proc ds ref)
  (lambda (js)
    (ref (unsafe-array-index->value-index ds js))))

(define/contract
  (array-strict? arr)
  (-> Array? boolean?)
  (unbox (Array-strict? arr)))

(define/contract
  (array-strict! arr)
  (-> Array? void?)
  (define strict? (Array-strict? arr))
  (unless (unbox strict?)
    ((Array-strict! arr))
    (set-box! strict? #t)))

(define/contract
  (array-default-strict! arr)
  (-> Array? void?)
  (define strict? (Array-strict? arr))
  (when (and (not (unbox strict?)) (array-strictness))
    ((Array-strict! arr))
    (set-box! strict? #t)))

(define/contract
  (unsafe-build-array ds f)
  (-> vector? (-> vector? any/c) Array?)
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
                      (unsafe-vector-ref vs (unsafe-array-index->value-index ds js))))))
    (define unsafe-proc
      (lambda (js) ((unbox f) js)))
    (Array ds size (box #f) strict! unsafe-proc)))

(define/contract
  (unsafe-build-simple-array ds f)
  (-> vector? (-> vector? any/c) Array?)
  (define size (check-array-shape-size 'unsafe-build-simple-array ds))
  (Array ds size (box #t) void f))

(define/contract
  (build-array ds proc)
  (-> vector? (-> vector? any/c) Array?)
  (let ([ds  (check-array-shape
              ds (lambda () (raise-argument-error 'build-array "(Vectorof Index)" 0 ds proc)))])
    (define arr
      (unsafe-build-array ds (lambda (js)
                               (proc (vector->immutable-vector js)))))
    (array-default-strict! arr)
    arr))

(define/contract
  (build-simple-array ds proc)
  (-> none/c none/c none/c)
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

