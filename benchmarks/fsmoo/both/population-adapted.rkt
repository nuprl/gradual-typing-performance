#lang typed/racket

(provide
 oPopulation
 build-random-population
)
(require
  "automata-adapted.rkt"
  require-typed-check)

(require/typed/check "population.rkt"
 (build-random-population
  (-> Natural oPopulation)))

(define-type Automaton* (Vectorof oAutomaton))
(define-type oPopulation (Instance Population))
(define-type Population
  (Class
   (init-field (a* Automaton*) (b* Automaton* #:optional))
   (payoffs (-> [Listof Payoff]))
   (match-up*
    ;; (match-ups p r) matches up neighboring pairs of
    ;; automata in population p for r rounds 
    (-> Natural Void))

   (death-birth
    ;; (death-birth p r) replaces r elements of p with r "children" of 
    ;; randomly chosen fittest elements of p, also shuffle 
    ;; constraint (< r (length p))
    (-> Natural [#:random (U False Payoff)] Void))))
