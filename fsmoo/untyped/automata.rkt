#lang racket

;; An N-states, N-inputs Automaton

;; type  Automaton = 
;;   Class
;;      [match-pair (-> Automata N (values Automata Automata))
;;      the sum of pay-offs for the two respective automata over all rounds
;; 
;;     [interact (-> Automaton (values Automaton Automaton))]
;;      give each automaton the reaction of the other in the current state
;;      determine payoff for each and transition the automaton
;; 
;;     [pay (-> Payoff)]
;;
;;     [reset (-> Automaton)]
;;     wipe out the historic payoff
;;
;;     [clone (-> Automaton)]
;;     create new automaton from given one (same original state)
;;
;;     [equal (-> Automaton)]

(provide
 ;; type Automaton
 ;; type Payoff = N 
 
 ;; Payoff -> Automaton 
 defects
 cooperates
 tit-for-tat
 grim-trigger
 
 ;; N -> Automaton
 ;; (make-random-automaton n k) builds an n states x k inputs automaton
 ;; with a random transition table 
 make-random-automaton)

;; =============================================================================
(module+ test
  (provide
   ;; syntax: (check-payoffs? actual expected1 expected2)
   ;; runs actual, expects two automata, compares their payoffs with expected{i}
   check-payoffs?)
  
  (require rackunit)
  
  (define-syntax-rule
    (check-payoffs? actual expected1 expected2)
    (check-equal? (let-values ([(auto1 auto2) actual])
                    (list (send auto1 pay) (send auto2 pay)))
                  (list expected1 expected2))))

;; -----------------------------------------------------------------------------
;; Table      = [Vectorof n Transition])
;; Transition = [Vectorof n State]
;;            ~ [Vectorof [Input --> State]]
;;            ~ [State -> [Input --> State]]
;; State      = [0,n)
;; Input      = [0,n)
;; Payoff     = N

(define (make-random-automaton n)
  (new automaton%
       [current (random n)]
       [payoff 0]
       [table
        (build-vector n (lambda _ (build-vector n (lambda _ (random n)))))]))

;; Automaton = (instance automaton% State Payoff Table)
(define automaton%
  (let ()
    ;; static [measure overhead]
    ;; PayoffTable = [Vectorof [Vectorof (cons Payoff Payoff)]]
    ;;             ~ [Input -> [Input -> (cons Payoff Payoff)]]
    (define PAYOFF-TABLE
      (vector (vector (cons 3 3) (cons 0 4))
              (vector (cons 4 0) (cons 1 1))))
    
    (class object%
      (init-field
       current ;; State 
       payoff  ;; Payoff 
       table   ;; [Vectorof [Vectorof State]] 
       (original current))
      (super-new)
      
      (define/public (match-pair other r)
        (for ([_i (in-range r)])
          (define input (get-field current other))
          (match-define (cons p1 p2) (compute-payoffs input))
          (jump input p1)
          (send other jump current p2))
        (values this other))
      
      ;; State Payoff -> Void
      (define/public (jump input delta) ;; <--- should be friendly
        (set! current (vector-ref (vector-ref table current) input))
        (set! payoff (+ payoff delta)))
      
      (define/public (pay)
        payoff)
      
      (define/public (reset)
        (new automaton% [current original][payoff 0][table table]))
      
      (define/public (clone)
        (new automaton% [current original][payoff 0][table table]))
      
      ;; State -> [Cons Payoff Payoff]
      (define/private (compute-payoffs other-current)
        (vector-ref (vector-ref PAYOFF-TABLE current) other-current))
      
      (define/public (equal other)
        (and (= current (get-field current other))
             (= original (get-field original other))
             (= payoff (get-field payoff other))
             (equal? table (get-field table other))))
      
      (define/public (guts)
        (list current original payoff table)))))


(define COOPERATE 0)
(define DEFECT    1)

(define (defects p0)
  (new automaton%
       [current DEFECT]
       [payoff p0]
       [table
        (transitions
         #:i-cooperate/it-cooperates DEFECT 
         #:i-cooperate/it-defects    DEFECT
         #:i-defect/it-cooperates    DEFECT
         #:i-defect/it-defects       DEFECT)]))

(define (cooperates p0)
  (new automaton%
       [current COOPERATE]
       [payoff p0]
       [table
        (transitions
         #:i-cooperate/it-cooperates COOPERATE 
         #:i-cooperate/it-defects    COOPERATE
         #:i-defect/it-cooperates    COOPERATE
         #:i-defect/it-defects       COOPERATE)]))

(define (tit-for-tat p0)
  (new automaton%
       [current COOPERATE]
       [payoff p0]
       [table
        (transitions
         #:i-cooperate/it-cooperates COOPERATE 
         #:i-cooperate/it-defects    DEFECT
         #:i-defect/it-cooperates    COOPERATE
         #:i-defect/it-defects       DEFECT)]))

(define (grim-trigger p0)
  (new automaton%
       [current COOPERATE]
       [payoff p0]
       [table
        (transitions
         #:i-cooperate/it-cooperates COOPERATE 
         #:i-cooperate/it-defects    DEFECT
         #:i-defect/it-cooperates    DEFECT
         #:i-defect/it-defects       DEFECT)]))

(define (transitions #:i-cooperate/it-cooperates cc
                     #:i-cooperate/it-defects    cd
                     #:i-defect/it-cooperates    dc
                     #:i-defect/it-defects       dd)
  (vector (vector cc cd) (vector dc dd)))

;; -----------------------------------------------------------------------------
(module+ test
  (define (make-automaton current table)
    (new automaton% [current current][payoff 0][table table]))
  
  (define t1
    (vector
     (vector 0 0)
     (vector 1 1)))
  (define t2
    (vector
     (vector 0 1)
     (vector 0 1)))
  (define observably-equivalent-to-all-defects (make-automaton DEFECT t1))
  (define observably-equivalent-to-tit-for-tat (make-automaton COOPERATE t2))
  
  (check-pred (lambda (x) (is-a? x automaton%)) (make-automaton 0 t1))
  
  (check-equal?
   (get-field
    payoff
    (send (new automaton% [current 1] [payoff 4] [table t2]) reset))
   0)
  
  (check-equal?
   (get-field
    current
    (send (new automaton% [current 1][payoff 4][table t2]) clone))
   1)
  
  
  (check-payoffs? (send (defects 0) match-pair (cooperates 0) 10) 40 0)
  (check-payoffs? (send (defects 0) match-pair (tit-for-tat 0) 10) 13 9)
  (check-payoffs? (send (tit-for-tat 0) match-pair (defects 0) 10) 9 13))