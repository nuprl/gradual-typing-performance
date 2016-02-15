#lang typed/racket/base

;;bg; My own imitation of in-array

(provide
  for-array/vector
)

(require
  "typed-data.rkt"
  (only-in "array-struct.rkt"
           array?
           array-shape
           unsafe-array-proc
           array-size
           array-strictness)
         (only-in racket/unsafe/ops unsafe-fx+ unsafe-fx<)
         (only-in "array-utils.rkt" next-indexes!))
(require (for-syntax racket/base syntax/parse))


(define-sequence-syntax in-array-clause
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

(: for-array/vector (-> Array (-> Real Integer) (Vectorof Integer)))
(define (for-array/vector arr f)
  (for/vector : (Vectorof Integer)
    #:length (array-size arr) ([sample (in-array-clause arr)])
    (f sample)))

