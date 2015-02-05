#lang racket/base

(require (only-in "../unsafe.rkt"
                  unsafe-vector-ref
                  unsafe-vector-set!
                  unsafe-fx+
                  unsafe-fx*))

(provide for-each-array-index
         inline-build-array-data)

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
         [(1)  (define d0 (unsafe-vector-ref ds 0))
               (let j0-loop ([j0 0])
                 (when (j0 . < . d0)
                   (unsafe-vector-set! js 0 j0)
                   (f js j0)
                   (j0-loop (+ j0 1))))]
         [(2)  (define d0 (unsafe-vector-ref ds 0))
               (define d1 (unsafe-vector-ref ds 1))
               (let j0-loop ([j0 0]
                                  [j 0])
                 (when (j0 . < . d0)
                   (unsafe-vector-set! js 0 j0)
                   (let j1-loop ([j1 0]
                                     [j j])
                     (cond [(j1 . < . d1)
                            (unsafe-vector-set! js 1 j1)
                            (f js j)
                            (j1-loop (+ j1 1) (unsafe-fx+ j 1))]
                           [else
                            (j0-loop (+ j0 1) j)]))))]
         [else  (let i-loop ([i  0]
                                                   [j  0])
                  (cond [(i . < . dims)
                        (define di (unsafe-vector-ref ds i))
                         (let ji-loop ([ji 0]
                                                             [j  j])
                           (cond [(ji . < . di)
                                  (unsafe-vector-set! js i ji)
                                  (ji-loop (+ ji 1) (i-loop (+ i 1) j))]
                                 [else  j]))]
                        [else  (f js j)
                               (unsafe-fx+ j 1)]))
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
         [(1)  (define d0 (unsafe-vector-ref ds 0))
               (let j0-loop ([j0   0])
                 (when (j0 . < . d0)
                   (unsafe-vector-set! js 0 j0)
                   (f js)
                   (j0-loop (+ j0 1))))]
         [(2)  (define d0 (unsafe-vector-ref ds 0))
               (define d1 (unsafe-vector-ref ds 1))
               (let j0-loop ([j0  0])
                 (when (j0 . < . d0)
                   (unsafe-vector-set! js 0 j0)
                   (let j1-loop  ([j1   0])
                     (cond [(j1 . < . d1)
                            (unsafe-vector-set! js 1 j1)
                            (f js)
                            (j1-loop (+ j1 1))]
                           [else
                            (j0-loop (+ j0 1))]))))]
         [else  (let i-loop  ([i   0])
                  (cond [(i . < . dims)
                         (define di  (unsafe-vector-ref ds i))
                         (let ji-loop  ([ji   0])
                           (when (ji . < . di)
                             (unsafe-vector-set! js i ji)
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
         [(1)  (define d0  (unsafe-vector-ref ds 0))
               (let j0-loop  ([j0   0])
                 (when (j0 . < . d0)
                   (f j0)
                   (j0-loop (+ j0 1))))]
         [(2)  (define d0  (unsafe-vector-ref ds 0))
               (define d1  (unsafe-vector-ref ds 1))
               (let j0-loop  ([j0   0]
                                     [j   0])
                 (when (j0 . < . d0)
                   (let j1-loop  ([j1   0]
                                         [j   j])
                     (cond [(j1 . < . d1)
                            (f j)
                            (j1-loop (+ j1 1) (unsafe-fx+ j 1))]
                           [else
                            (j0-loop (+ j0 1) j)]))))]
         [else  (let i-loop  ([i   0]
                                                   [j   0])
                  (cond [(i . < . dims)
                         (define di  (unsafe-vector-ref ds i))
                         (let ji-loop  ([ji   0]
                                                             [j   j])
                           (cond [(ji . < . di)
                                  (ji-loop (+ ji 1) (i-loop (+ i 1) j))]
                                 [else  j]))]
                        [else  (f j)
                               (unsafe-fx+ j 1)]))
                (void)])])))

(define-syntax-rule (inline-build-array-data ds-expr g-expr A)
  (let* ([ds   ds-expr]
          [dims   (vector-length ds)])
    (define-syntax-rule (g js j)
      (g-expr js j))
    (define size 
      (let loop  ([k   0] [size   1])
        (cond [(k . < . dims)  (loop (+ k 1) (unsafe-fx* size (unsafe-vector-ref ds k)))]
              [else  size])))
    (cond [(= size 0)  (vector) ]
          [else
           (define js0  (make-vector dims 0))
           (define vs (make-vector size (g js0 0)))
           (for-each-array+data-index ds (λ (js j) (unsafe-vector-set! vs j (g js j))))
           vs])))
