#lang racket/base

;; Statistical helpers

(provide
  anderson-darling anderson-darling?
)

(require
  (only-in math/statistics
    mean)
  (only-in math/special-functions
    erf)
  (only-in racket/list
    take)
  (only-in racket/string
    string-suffix?)
  (only-in racket/file
    file->value)
)

;; -----------------------------------------------------------------------------
;; Anderson-Darling normality test
;; https://projecteuclid.org/download/pdf_1/euclid.aos/1176343411

(define (sample-variance/mean+length x* u n)
  (* (/ 1 (- n 1))
     (for/sum ([x (in-list x*)])
       (expt (- x u) 2))))

(define (sample-stddev/mean+length x* u n)
  (sqrt (sample-variance/mean+length x* u n)))

;; CDF for Normal Distribution
(define ((make-phi u o) n)
  (* (/ 1 2)
     (+ 1
        (erf (/ (- n u)
                (* o (sqrt 2)))))))

;; Calculate Anderson-Darling statistic A**2
(define (anderson-darling x*-raw)
  (define n (length x*))
  (define x* (sort (take x*-raw n) <))
  (define u (mean x*))
  (define o (sample-stddev/mean+length x* u n))
  (define phi (make-phi u o))
  (define y* (for/vector ([x (in-list x*)])
               (/ (- x u) o)))
  (- (- n)
     (* (/ 1 n)
        (for/sum ([i (in-range n)])
          (* (- (* 2 i) 1)
             (+ (log (phi (vector-ref y* i)))
                (log (- 1 (phi (vector-ref y* (- n 1 i)))))))))))

(define (anderson-darling? x*)
  (define A**2 (anderson-darling x*))
  (not (> A**2 1.0)))

