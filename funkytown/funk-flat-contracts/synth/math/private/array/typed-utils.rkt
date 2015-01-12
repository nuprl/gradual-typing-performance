#lang racket/base

(require racket/fixnum
         racket/list
         racket/performance-hint
         (for-syntax racket/base)
         "../unsafe.rkt"
         racket/contract)

(define/contract
  (index? n)
  (-> natural-number/c boolean?)
  (and (<= 0 n)
       (<  n 999999999999)))

(provide (all-defined-out))

(begin-encourage-inline
  
  (define/contract
    (vector->supertype-vector js)
    (-> vector? vector?)
    (define dims (vector-length js))
    (cond [(= dims 0)  (vector)]
          [else  (define new-js (make-vector dims (unsafe-vector-ref js 0)))
                 (let loop ([i 1])
                   (cond [(i . < . dims)  (unsafe-vector-set! new-js i (unsafe-vector-ref js i))
                                          (loop (+ i 1))]
                         [else  new-js]))]))
  
  (define/contract
    (vector-copy-all js)
    (-> vector? vector?)
    (vector->supertype-vector js))
  
  (define/contract
    (array-shape-size ds)
    (-> vector? natural-number/c)
    (define dims (vector-length ds))
    (let loop ([i 0] [n 1])
      (cond [(i . < . dims)  (define d (unsafe-vector-ref ds i))
                             (loop (+ i 1) (* n d))]
            [else  n])))
  
  (define/contract
    (check-array-shape-size name ds)
    (-> symbol? vector? natural-number/c)
    (define size (array-shape-size ds))
    (cond [(index? size)  size]
          [else  (error name "array size ~e (for shape ~e) is too large (is not an Index)" size ds)]))
  
  (define/contract
    (check-array-shape ds fail)
    (-> vector? (-> none/c) vector?)
    (define dims (vector-length ds))
    (define new-ds  (make-vector dims 0))
    (let loop ([i 0])
      (cond [(i . < . dims)
             (define di (unsafe-vector-ref ds i))
             (cond [(index? di)  (unsafe-vector-set! new-ds i di)
                                 (loop (+ i 1))]
                   [else  (fail)])]
            [else  new-ds])))
  
  (define/contract
    (unsafe-array-index->value-index ds js)
    (-> vector? vector? natural-number/c)
    (define dims (vector-length ds))
    (let loop ([i 0] [j 0])
      (cond [(i . < . dims)
             (define di (unsafe-vector-ref ds i))
             (define ji (unsafe-vector-ref js i))
             (loop (+ i 1) (unsafe-fx+ ji (unsafe-fx* di j)))]
            [else  j])))
  
  
  )  ; begin-encourage-inline

(define/contract
  (raise-array-index-error name ds js)
  (-> symbol? vector? any/c none/c)
  (error name "expected indexes for shape ~e; given ~e"
         (vector->list ds) js))

(define/contract
  (array-index->value-index name ds js)
  (-> none/c none/c none/c none/c)
  (define (raise-index-error) (raise-array-index-error name ds js))
  (define dims (vector-length ds))
  (unless (= dims (vector-length js)) (raise-index-error))
  (let loop ([i 0] [j  0])
    (cond [(i . < . dims)
           (define di (unsafe-vector-ref ds i))
           (define ji (unsafe-vector-ref js i))
           (cond [(and (exact-integer? ji) (0 . <= . ji) (ji . < . di))
                  (loop (+ i 1) (unsafe-fx+ ji (unsafe-fx* di j)))]
                 [else  (raise-index-error)])]
          [else  j])))

(define/contract
  (check-array-indexes name ds js)
  (-> none/c none/c none/c none/c)
  (define (raise-index-error) (raise-array-index-error name ds js))
  (define dims (vector-length ds))
  (unless (= dims (vector-length js)) (raise-index-error))
  (define new-js  (make-vector dims 0))
  (let loop ([i 0])
    (cond [(i . < . dims)
           (define di (unsafe-vector-ref ds i))
           (define ji (unsafe-vector-ref js i))
           (cond [(and (exact-integer? ji) (0 . <= . ji) (ji . < . di))
                  (unsafe-vector-set! new-js i ji)
                  (loop (+ i 1))]
                 [else  (raise-index-error)])]
          [else  new-js])))

(define/contract
  (unsafe-vector-remove vec k)
  (-> vector? natural-number/c vector?)
  (define n (vector-length vec))
  (define n-1 (sub1 n))
  (cond
    [(not (index? n-1)) (error 'unsafe-vector-remove "internal error")]
    [else
     (define new-vec  (make-vector n-1 (unsafe-vector-ref vec 0)))
     (let loop ([i 0])
       (when (i . < . k)
         (unsafe-vector-set! new-vec i (unsafe-vector-ref vec i))
         (loop (+ i 1))))
     (let loop ([i k])
       (cond [(i . < . n-1)
              (unsafe-vector-set! new-vec i (unsafe-vector-ref vec (+ i 1)))
              (loop (+ i 1))]
             [else  new-vec]))]))

(define/contract
  (unsafe-vector-insert vec k v)
  ;(parametric->/c (A) (-> (vectorof A) natural-number/c A (vectorof A)))
  (-> vector? natural-number/c any/c vector?)
  (define n (vector-length vec))
  (define dst-vec (make-vector (+ n 1) v))
  (let loop ([i 0])
    (when (i . < . k)
      (unsafe-vector-set! dst-vec i (unsafe-vector-ref vec i))
      (loop (+ i 1))))
  (let loop ([i k])
    (when (i . < . n)
      (let ([i+1  (+ i 1)])
        (unsafe-vector-set! dst-vec i+1 (unsafe-vector-ref vec i))
        (loop i+1))))
  dst-vec)

(define/contract
  (make-thread-local-indexes dims)
  (-> none/c none/c)
  (let ([val (make-thread-cell #f)])
    (λ () (or (thread-cell-ref val)
              (let ([v   (make-vector dims 0)])
                (thread-cell-set! val v)
                v)))))

;; Sets js to the next vector of indexes, in row-major order
(define/contract
  (next-indexes! ds dims js)
  (-> vector? natural-number/c vector? void?)
  (let loop ([k  dims])
    (unless (zero? k)
      (let ([k  (- k 1)])
        (define jk (unsafe-vector-ref js k))
        (define dk (unsafe-vector-ref ds k))
        (let ([jk  (+ jk 1)])
          (cond [(jk . >= . dk)
                 (unsafe-vector-set! js k 0)
                 (loop k)]
                [else
                 (unsafe-vector-set! js k jk)]))))))
