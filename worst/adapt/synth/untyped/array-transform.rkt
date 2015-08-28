#lang racket/base

(require racket/vector
         (only-in racket/fixnum fx+)
         benchmark-util
         "array-struct.rkt")

(require "array-broadcast.rkt"
  "array-utils.rkt")

(provide array-append*)

(define index? exact-nonnegative-integer?)

;(: array-broadcast-for-append (-> (Listof Array)
;                                        Integer (values (Listof Array)
;                                                           (Listof Integer))))
(define (array-broadcast-for-append arrs k)
  ;(: dss (Listof Indexes))
  (define dss
    (for/list ;: (Listof Indexes)
      ([arr  arrs])
      (array-shape arr)))
  ;(: dims Natural)
  (define dims (apply max (for/list ;: (Listof Natural)
                            ([ds  dss])
                            (vector-length ds))))
  ;(define dims (apply max (map vector-length dss)))
  (cond [(not (index? dims))  (error 'array-broadcast-for-append "can't happen")]
        [(or (k . < . 0) (k . >= . dims))
         (raise-argument-error 'array-append* (format "Integer < ~a" dims) k)]
        [else
         (let* ([dss  (map (λ (ds)
                             (define dms (vector-length ds))
                             (vector-append (make-vector (- dims dms) 1) ds))
                           dss)]
                [dks  (map (λ (ds) (vector-ref ds k)) dss)]
                [dss  (map (λ (ds) (unsafe-vector-remove ds k)) dss)]
                [ds   (array-shape-broadcast dss)]
                [dss  (map (λ (dk) (unsafe-vector-insert ds k dk)) dks)])
           (define new-arrs
             (map (λ (arr ds) (array-broadcast arr ds)) arrs dss))
           (values new-arrs dks))]))

;(: array-append* (-> (Listof Array) Array))
(define (array-append* arrs [k 0])
  (when (null? arrs) (raise-argument-error 'array-append* "nonempty (Listof Array)" arrs))
  (let-values ([(arrs dks)  (array-broadcast-for-append arrs k)])
    (define new-dk (apply + dks))
    (cond
      [(not (index? new-dk))  (error 'array-append* "resulting axis is too large (not an Integer)")]
      [else
       ;(: dss (Listof Indexes))
       (define dss (map (λ (arr) (array-shape arr)) arrs))
       ;(: new-ds Indexes)
       (define new-ds (vector-copy-all (car dss)))
       (vector-set! new-ds k new-dk)
       ;; Make two mappings:
       ;; 1. old-procs : new array index -> old array procedure
       ;; 2. old-jks :   new array index -> old array index
       (define old-procs (make-vector new-dk (unsafe-array-proc (car arrs))))
       (define old-jks (make-vector new-dk 0))
       (let arrs-loop ([arrs arrs] [dks dks] [jk  0])
         (unless (null? arrs)
           (define arr (car arrs))
           (define proc (unsafe-array-proc arr))
           (define dk (car dks))
           (let i-loop ([i  0] [jk jk])
             (cond [(i . < . dk)  (vector-set! old-procs jk proc)
                                  (vector-set! old-jks jk i)
                                  (i-loop (+ i 1) (fx+ jk 1))]
                   [else  (arrs-loop (cdr arrs) (cdr dks) jk)]))))
       ;(: arr* Array)
       (define arr*
        (unsafe-build-array
         new-ds (λ (js)
                  (define jk (vector-ref js k))
                  (vector-set! js k (vector-ref old-jks jk))
                  (define v ((vector-ref old-procs jk) js))
                  (vector-set! js k jk)
                  v)))
       (array-default-strict! arr*)
       arr*])))
