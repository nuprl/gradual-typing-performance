#lang racket

(provide
 automaton?
 ;; type Payoff = PositiveNumber 
 ;Payoff
 ;Automaton

 ;; -> [Population Automata]
 A
 ;; Automata Automata -> Payoff Payoff Automata Automata 
 interact

 ;; --
 (struct-out Population)
 build-population
 match-ups
 death-birth
)

;; ---------------------------------------------------------------------------------------------------

(require "population.rkt" "utilities.rkt")

;; AUTOMATON
;(define-type Payoff Real)

;(define-type Event (U 0 1))
(define COOPERATE 0)
(define DEFECT    1)

;(define-type Name Event)

(struct automaton (
  current-state ;: Name}
  states ;: [Vectorof state]}
) #:transparent #:mutable)
(struct state     (
  name ;: Name}
  actions ;: [Vectorof action]}
) #:transparent #:mutable)
(struct action    (
  event ;: Event)
  result ;: Name}
) #:transparent #:mutable)

;(define-type Automaton automaton)

;(: A (-> [Population Automaton]))
(define (A)
  (build-population
   100
   (lambda (_)
     (create (one-of COOPERATE DEFECT)
             (one-of COOPERATE DEFECT)
             (one-of COOPERATE DEFECT)
             (one-of COOPERATE DEFECT)
             (one-of COOPERATE DEFECT)))))

;(: create (-> Name Name Name Name Name Automaton))
(define (create seed a000 a001 a100 a101)
  (define state1 (state COOPERATE (vector (action COOPERATE a000) (action DEFECT a001))))
  (define state2 (state DEFECT    (vector (action COOPERATE a100) (action DEFECT a101))))
  (automaton seed (vector state1 state2)))

;(: interact (-> automaton automaton (values Payoff Payoff Automaton Automaton)))
(define (interact auto1 auto2)
  (match-define (automaton strat1 states1) auto1)
  (match-define (automaton strat2 states2) auto2)
  [define-values (payoff1 payoff2) (match-strategies strat1 strat2)]
  (define actions1 (state-actions (vector-ref states1 strat1)))
  (define next1    (action-result (vector-ref actions1 strat2)))
  (define actions2 (state-actions (vector-ref states2 strat2)))
  (define next2    (action-result (vector-ref actions2 strat1)))
  (values payoff1 payoff2 (automaton next1 states1) (automaton next2 states2)))

;(: match-strategies (-> Event Event (values Payoff Payoff)))
(define (match-strategies strat1 strat2)
  (cond 
    [(and (equal? COOPERATE strat1) (equal? COOPERATE strat2)) (values 3 3)]
    [(and (equal? COOPERATE strat1) (equal? DEFECT strat2))    (values 0 4)]
    [(and (equal? DEFECT strat1) (equal? COOPERATE strat2))    (values 4 0)]
    [else                                                      (values 1 1)]))

;; ---------------------------------------------------------------------------------------------------
;; CLASSIC AUTOMATA
;(: all-defects Automaton)
(define all-defects (create COOPERATE DEFECT DEFECT DEFECT DEFECT))

;(: all-cooperates Automaton)
(define all-cooperates (create DEFECT COOPERATE COOPERATE COOPERATE COOPERATE))

;; the tit-for-tat strategy starts out optimistic:
;; it cooperates initially
;; if the opponent defects, it switches to defecting
;; if the opponent cooperates, it plays cooperately
;(: tit-for-tat Automaton)
(define tit-for-tat (create COOPERATE COOPERATE DEFECT COOPERATE DEFECT))

;; the grim trigger also starts out optimistic,
;; but the opponent defects for just once then
;; it jumps to defect forever
;; it doesnt forgive, and doesnt forget
;(: grim-trigger Automaton)
(define grim-trigger (create 0 0 1 1 1))
