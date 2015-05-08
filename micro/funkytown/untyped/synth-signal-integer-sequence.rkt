#lang racket/base

(provide signal->integer-sequence)

;; -----------------------------------------------------------------------------

(require (for-syntax racket/base syntax/parse)
         (only-in racket/unsafe/ops unsafe-fx+ unsafe-fx<)
         (only-in racket/math exact-floor)
         "data-array.rkt"
(only-in "synth-constants.rkt"
  bits-per-sample)
 (only-in"array-utils-next-indexes.rkt"
  next-indexes!))

;; =============================================================================

;; assumes array of floats in [-1.0,1.0]
;; assumes gain in [0,1], which determines how loud the output is
(define (signal->integer-sequence signal #:gain [gain 1])
  (for/vector #:length (Array-size signal)
              ([sample (in-array signal)])
    (max 0 (min (sub1 (expt 2 bits-per-sample)) ; clamp
                (exact-floor
                 (* gain
                    (* (+ sample 1.0) ; center at 1, instead of 0
                       (expt 2 (sub1 bits-per-sample)))))))))

;; -----------------------------------------------------------------------------

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
               (cond [(Array? arr)
                      (define ds (Array-shape arr))
                      (define dims (vector-length ds))
                      (define size (Array-size arr))
                      (define proc (Array-unsafe-proc arr))
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
