#lang typed/racket

(provide
 Automaton
 oAutomaton
 Payoff
 make-random-automaton
)

(require require-typed-check)
(require/typed/check "automata.rkt"
 (make-random-automaton
  (-> Natural oAutomaton)))

(define-type Payoff Nonnegative-Real)
(define-type Transition* [Vectorof [Vectorof State]])
(define-type State Natural)
(define-type Input Natural)
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
