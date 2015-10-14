#lang typed/racket/base

(require
  benchmark-util
)
(define-type [Population a] (cons (Vectorof a) (Vectorof a)))
(require/typed/check "automata.rkt"
 [#:opaque Automaton automaton?] ;; automata? is unbound for typed programs but THATS FINE because of r/t/c implementation
 (A (-> [Population Automaton]))
 (interact (-> Automaton Automaton (values Payoff Payoff Automaton Automaton)))
)

(define-type Payoff Real)

(provide
  Payoff
  Automaton
  A
  interact
)

