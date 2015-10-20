#lang racket

;; Populations of Automata

;; population-payoffs (-> [Listof Payoff])

;; match-up* (-> N Population)
;; (match-ups p r) matches up neighboring pairs of
;; automata in population p for r rounds 
;; 

;; death-birth N -> Population 
;; (death-birth p r) replaces r elements of p with r "children" of 
;; randomly chosen fittest elements of p, also shuffle 
;; constraint (< r (length p))

(provide
 ;; type Population
 
 ;; N -> Population
 ;; (build-population n c) for even n, build a population of size n 
 ;; with c constraint: (even? n)
 build-random-population
 
 )

;; =============================================================================
(require "automata.rkt" "utilities.rkt")

(module+ test
  (require rackunit)
  (require  (submod "automata.rkt" test)))

;; Population = (Cons Automaton* Automaton*)
;; Automaton* = [Vectorof Automaton]

(define DEF-COO 2)

;; -----------------------------------------------------------------------------
(define (build-random-population n)
  (define v (build-vector n (lambda (_) (make-random-automaton DEF-COO))))
  (new population% [a* v]))

(define population%
  (class object%
    (init-field a* (b* a*))
    (super-new)
    
    (define/public (payoffs)
      (for/list ([a a*]) (send a pay)))
    
    (define/public (match-up* rounds-per-match)
      ;; comment out this line if you want cummulative payoff histories:
      ;; see below in birth-death
      (reset)
      ;; -- IN --
      (for ([i (in-range 0 (- (vector-length a*) 1) 2)])
        (define p1 (vector-ref a* i))
        (define p2 (vector-ref a* (+ i 1)))
        (define-values (a1 a2) (send p1 match-pair p2 rounds-per-match))
        (vector-set! a* i a1)
        (vector-set! a* (+ i 1) a2))
      this)
    
    (define/public (death-birth rate #:random (q #false))
      (define payoffs (for/list ([x (in-vector a*)]) (send x pay)))
      [define substitutes (choose-randomly payoffs rate #:random q)]
      (for ([i (in-range rate)][p (in-list substitutes)])
        (vector-set! a* i (send (vector-ref b* p) clone)))
      (shuffle-vector))
    
    ;; -> Void
    ;; effec: reset all automata in a*
    (define/private (reset)
      (for ([x a*][i (in-naturals)]) (vector-set! a* i (send x reset))))
    
    ;; -> Population
    ;; effect: shuffle vector b into vector a
    ;; constraint: (= (vector-length a) (vector-length b))
    ;; Fisher-Yates Shuffle
    
    (define/private (shuffle-vector)
      ;; copy b into a
      (for ([x (in-vector a*)][i (in-naturals)])
        (vector-set! b* i x))
      ;; now shuffle a 
      (for ([x (in-vector a*)] [i (in-naturals)])
        (define j (random (add1 i)))
        (unless (= j i) (vector-set! b* i (vector-ref b* j)))
        (vector-set! b* j x))
      (define tmp a*)
      (set! a* b*)
      (set! b* tmp)
      this)))

;; -----------------------------------------------------------------------------
(module+ test
  (define a1 (vector (defects 0) (cooperates 40)))
  (define p1 (new population% [a* a1]))
  (define e1 (vector (defects 40) (cooperates 0)))
  (define p1-expected (cons e1 a1))
  
  (define a2 (vector (defects 0) (tit-for-tat 0)))
  (define p2 (cons a2 a2))
  (define e2 (vector (defects 13) (tit-for-tat 9)))
  (define p2-expected (cons e2 a2))
  
  (define a3 (vector (tit-for-tat 0) (defects 0)))
  (define p3 (cons a3 a3))
  (define e3 (vector (tit-for-tat 9) (defects 13)))
  (define p3-expected (cons e3 a3))
  
  ;; these don't work because the population changes 
  ; (check-euqal? (match-up* p2 10) p2-expected)
  ; (check-equal? (match-up* p3 10) p3-expected)  
  (check-true
   (let ([p3-a (get-field a*(send p1 match-up* 10))])
     (for/and ([a p3-a][e e1])
       (define x (send a equal e))
       (unless x (displayln `(,(send a guts) ,(send e guts))))
       x)))
  
  (define a* (vector (cooperates 1)))
  (define p* (new population% [a* a*]))
  (check-equal? (send p* death-birth 1) p*)
  
  (define a20 (vector (cooperates 1)  (cooperates 9)))
  (define p20 (new population% [a* a20]))
  
  (define c0 (cooperates 0))
  (define c9 (cooperates 9))
  (check-pred
   (lambda (o)
     (define a* (get-field a* o))
     (or
      (and
       (send (vector-ref a* 0) equal c0)
       (send (vector-ref a* 1) equal c9))
      (and
       (send (vector-ref a* 0) equal c9)
       (send (vector-ref a* 1) equal c0))))
   (send p20 death-birth 1 #:random .2)))

