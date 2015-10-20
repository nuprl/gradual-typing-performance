#lang typed/racket

;; An N-states, N-inputs Automaton

(define-type Automaton
  (Class
   (init-field [current State]
               [payoff Payoff] 
               [table Transition*] 
               [original State #:optional])
   [match-pair
    ;; the sum of pay-offs for the two respective automata over all rounds
    (-> oAutomaton Natural (values oAutomaton oAutomaton))]
   [jump
    ;; this has no business being public 
    (-> State Payoff Void)]
   [pay
    (-> Payoff)]
   [reset
    ;; reset the historic payoff 
    (-> oAutomaton)]
   [clone
    ;; reset payoff and current state to original strategy
    (-> oAutomaton)]
   [equal (-> oAutomaton Boolean)]))

(define-type oAutomaton (Instance Automaton))

(define-type Payoff Nonnegative-Real)

(require "../base/type-utility.rkt")

(provide
 Payoff
 oAutomaton)

(provide/type
 (defects (-> Payoff oAutomaton))
 (cooperates (-> Payoff oAutomaton))
 (tit-for-tat (-> Payoff oAutomaton))
 (grim-trigger (-> Payoff oAutomaton))
 
 (make-random-automaton
  ;; (make-random-automaton n k) builds an n states x k inputs automaton
  ;; with a random transition table 
  (-> Natural oAutomaton)))

;; =============================================================================
(module+ test
  (provide
   ;; syntax: (check-payoffs? actual expected1 expected2)
   ;; runs actual, expects two automata, compares their payoffs with expected{i}
   check-payoffs?)
  
  (require typed/rackunit)
  
  (define-syntax-rule
    (check-payoffs? actual expected1 expected2)
    (check-equal? (let-values ([(auto1 auto2) actual])
                    (list (send auto1 pay) (send auto2 pay)))
                  (list expected1 expected2))))

;; -----------------------------------------------------------------------------
(define-type Transition* [Vectorof [Vectorof State]])
(define-type State Natural)
(define-type Input Natural)

(define (make-random-automaton n)
  (: trans [-> Any [Vectorof State]])
  (define (trans _i) (build-vector n (lambda (_) (random n))))
  (define seed (random n))
  (new automaton% [current seed] [payoff 0] [table (build-vector n trans)]))

(: automaton% Automaton)
(define automaton%
  (let ()
    ;; static [measure overhead]
    (: PAYOFF-TABLE [Vectorof [Vectorof (cons Payoff Payoff)]])
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
        (for ([_i : Natural (in-range r)])
          (define input (get-field current other))
          (match-define (cons p1 p2) (compute-payoffs input))
          (jump input p1)
          (send other jump current p2))
        (values this other))
      
      (define/public (jump input delta) ;; <--- should be friendly
        (set! current (vector-ref (vector-ref table current) input))
        (set! payoff (+ payoff delta)))
      
      (define/public (pay)
        payoff)
      
      (define/public (reset)
        (new automaton% [current original][payoff 0][table table]))
      
      (define/public (clone)
        (new automaton% [current original][payoff 0][table table]))
      
      (: compute-payoffs (-> State [cons Payoff Payoff]))
      (define/private (compute-payoffs other-current)
        (vector-ref (vector-ref PAYOFF-TABLE current) other-current))
      
      (define/public (equal other)
        (and (= current (get-field current other))
             (= original (get-field original other))
             (= payoff (get-field payoff other))
             (equal? table (get-field table other)))))))


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

(: transitions (-> #:i-cooperate/it-cooperates State
                   #:i-cooperate/it-defects    State
                   #:i-defect/it-cooperates    State
                   #:i-defect/it-defects  State
                   Transition*))

(define (transitions #:i-cooperate/it-cooperates cc
                     #:i-cooperate/it-defects    cd
                     #:i-defect/it-cooperates    dc
                     #:i-defect/it-defects       dd)
  (vector (vector cc cd) (vector dc dd)))

;; -----------------------------------------------------------------------------
(module+ test
  (: make-automaton (-> State Transition* oAutomaton))
  (define (make-automaton current table)
    (new automaton% [current current][payoff 0][table table]))

  (: t1 Transition*)
  (define t1
    (vector
     (vector 0 0)
     (vector 1 1)))
  (: t2 Transition*)
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
  
  (define d0 (defects 0))
  (define c0 (cooperates 0))
  (check-payoffs? (send d0 match-pair c0 10) 40 0)
  (check-payoffs? (send (defects 0) match-pair (tit-for-tat 0) 10) 13 9)
  (check-payoffs? (send (tit-for-tat 0) match-pair (defects 0) 10) 9 13))
