#lang racket

;; Utility Functions

(provide
 ;; [Listof Number] -> Number 
 sum
 
 ;; [Listof Number] Number -> Number 
 relative-average
 
 ;; type Probability = NonNegativeReal 
 ;; [Listof Probability] N -> [Listof N]
 ;; choose n random indices i such i's likelihood is (list-ref probabilities i)
 choose-randomly)

;; =============================================================================
(module+ test
  (require rackunit))

;; -----------------------------------------------------------------------------
(module+ test
  (check-equal? (sum '(1 2 3)) 6))

(define (sum l)
  (apply + l))

;; -----------------------------------------------------------------------------
(module+ test
  (check-equal? (relative-average '(1 2 3) 1) 2.0)
  (check-equal? (relative-average '(1 2 3) 2) 1.0))

(define (relative-average l w)
  (exact->inexact
   (/ (sum l)
      w (length l))))

;; -----------------------------------------------------------------------------

(module+ test
  (define p0 (list 10 90))
  (check-equal? (choose-randomly p0 1 #:random .2) (list 1)))

(define (choose-randomly probabilities speed #:random (q #false))
  (define %s (accumulated-%s probabilities))
  (for/list ([n (in-range speed)])
    [define r (or q (random))]
    ;; population is non-empty so there will be some i such that ... 
    (for/last ([p (in-naturals)] [% (in-list %s)] #:final (< r %)) p)))

;; [Listof Probability] -> [Listof Probability]
;; calculate the accumulated probabilities 

(module+ test
  (check-equal? (accumulated-%s (list 1)) '(1.0))
  (check-equal? (accumulated-%s (list 2 2)) '(.5 1.0))
  (check-equal? (accumulated-%s (list 2 8)) '(.2 1.0)))

(define (accumulated-%s probabilities)
  (define total (sum probabilities))
  (let relative->absolute ([payoffs probabilities][so-far #i0.0])
    (cond
      [(empty? payoffs) '()]
      [else (define nxt (+ so-far (first payoffs)))
            (cons (/ nxt total) (relative->absolute (rest payoffs) nxt))])))
