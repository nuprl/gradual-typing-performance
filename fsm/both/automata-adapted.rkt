#lang typed/racket/base

(provide
  Payoff
  Automaton
  A
  interact
)

(require
  benchmark-util
  "population-adapted.rkt"
)
(require/typed/check "automata.rkt"
 [#:opaque Automaton automaton?]
 ;[set-current-state! (-> Automaton Name Void)]
 ;[set-states! (-> Automaton (Vectorof state) Void)]
 (A (-> [Population Automaton]))
 (interact (-> Automaton Automaton (values Payoff Payoff Automaton Automaton)))
)

(define-type Payoff Real)
