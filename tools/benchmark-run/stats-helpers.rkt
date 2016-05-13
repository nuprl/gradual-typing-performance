#lang racket/base

;; Statistical helpers

(provide
  anderson-darling
  ;; (-> (Listof Natural) Float)
  ;; Compute the Anderson-Darling statistic for a list of numbers

  anderson-darling?
  ;; (-> (Listof Natural) Boolean)
  ;; True if the input data is most-likely normal.
  ;;  (p=1%, assuming at least 10 samples)
)

(require
  math/statistics
  (only-in math/special-functions
    erf)
  (only-in racket/math
    nan?)
)

;; -----------------------------------------------------------------------------
;; Anderson-Darling normality test
;; http://www.hep.caltech.edu/~fcp/statistics/hypothesisTest/PoissonConsistency/AndersonDarling1954.pdf

(define (sample-variance/mean+length x* u n)
  (* (/ 1 (- n 1))
     (for/sum ([x (in-list x*)])
       (expt (- x u) 2))))

(define (sample-stddev/mean+length x* u n)
  (sqrt (sample-variance/mean+length x* u n)))

;; CDF for Normal Distribution with mean `u` and stddev `o`
(define ((make-phi u o) x)
  (* (/ 1 2)
     (+ 1
        (erf (/ (- x u)
                (* o (sqrt 2)))))))

(define phi (make-phi 0 1))

(define (z-scores x*)
  (define n (length x*))
  ;; u = sample mean
  (define u (mean x*))
  ;; o = sample standard deviation
  (define o (sample-stddev/mean+length x* u n))
  (for/list ([x (in-list x*)])
    (/ (- x u) o)))

;; Calculate Anderson-Darling statistic A**2
(define (anderson-darling x*-unsorted)
  ;; x* = samples, in increasing order
  (define x* (sort x*-unsorted <))
  ;; z* = sample Z-scores, mapped through the standard normal CDF
  (define z* (list->vector (map phi (z-scores x*))))
  (define n (vector-length z*))
  ;; E = main summation for the A**2 statistic
  (define E
    (for/sum ([i (in-range n)])
      (* (- (* 2 (+ 1 i)) 1)
         (+ (log (vector-ref z* i))
            (log (- 1 (vector-ref z* (- n 1 i))))))))
  ;; A-D critical value
  (define A**2 (- (/ (- E) n) n))
  ;; Modified statistic, because mean & stddev are unknown
  (modify A**2 n))

;; Compute modified A-D statistic.
;; Stephens (1974) claims this is slightly better. So why not.
(define (modify x n)
  (* x
     (- (+ 1 (/ 4 n))
        (/ 25 (expt n 2)))))

;; Returns #t if the AD statistic is below the critical value
(define (anderson-darling? x*)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define A**2 (anderson-darling x*))
    (and (not (nan? A**2))
         (<= A**2 1.1))))

;; -----------------------------------------------------------------------------
;; Jarque-Bera normality test (needs many +20 samples)

(define (jarque-bera x*)
  (define u (mean x*))
  (define n (length x*))
  (define S (skewness/mean u x*))
  (define K (kurtosis/mean u x*))
  (* (/ 1 6)
     (+ (expt S 2)
        (* (/ 1 4)
           (expt (- K 3) 2)))))

(define (jarque-bera? x*)
  (> 0.1 (jarque-bera x*)))

;; -----------------------------------------------------------------------------

;; For spot-checking existing data files
(module+ main
  (require glob racket/string racket/format racket/list racket/file)
  (for ([fname (in-vector (current-command-line-arguments))])
   (define num-bad
    (for/sum ([x* (in-vector (file->value fname))]
              [i (in-naturals)])
      (if (anderson-darling? x*) 0 1)))
   (define tag (car (string-split (last (string-split fname "/")) "-")))
   (printf "~a : ~a (~a%)\n" fname num-bad (~r #:precision 2 (* 100 (/ num-bad (expt 2 (length (glob (format "~a/untyped/*.rkt" tag ))))))))))

