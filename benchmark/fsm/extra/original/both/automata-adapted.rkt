#lang typed/racket/base

(require
  benchmark-util
)
(require/typed/check "automata.rkt"
 [#:opaque Automaton automaton?] ;; automata? is unbound for typed programs but THATS FINE because of r/t/c implementation
 [#:opaque Population Population?]
 (A (-> Population))
 (interact (-> Automaton Automaton (values Payoff Payoff Automaton Automaton)))
 [build-population (-> Natural (-> Natural Automaton) Population)]
  [match-ups (-> Population Natural [-> Automaton Automaton (values Real Real Automaton Automaton)] [Listof Real])]
  [death-birth (-> Population [Listof Real] Natural Population)]
)

(define-type Payoff Real)

(provide
  Payoff
  Automaton
  A
  interact
  Population
  build-population
  match-ups
  death-birth
)

