#lang typed/racket

(provide
 ;; Population N N N -> [Listof Payoff]
 ;; (evolve p c s r) computes the list of average payoffs over the evolution of population p
 ;; for c cycles of of match-ups with r rounds per match and at death-birth rate of s
 evolve)

;; ---------------------------------------------------------------------------------------------------
(require
  benchmark-util
  "automata-adapted.rkt"
  "population-adapted.rkt"
)
(require/typed/check "utilities.rkt"
  [sum (-> (Listof Real) Real)]
  [relative-average (-> (Listof Real) Real Real)]
)

(: evolve (-> [Population Automaton] Natural Natural Natural [Listof Payoff]))
(define (evolve population cycles rate rounds)
  (define-values (result _)
    (for/fold ([result : [Listof Payoff] '()][population : [Population Automaton] population])
              ([_ (in-range cycles)])
      [define payoffs ((inst match-ups Automaton) population rounds interact)]
      (values ({inst cons Real [Listof Payoff]} (relative-average payoffs rounds) result)
              (death-birth population (payoff-percentages payoffs) rate))))
  (reverse result))

(: payoff-percentages (-> [Listof Payoff] [Listof Real]))
;; constraint: (= (sum payoff) payoff-sum)
;; from the matching result, calculate the accumulated fitness
(define (payoff-percentages payoffs)
  (define payoff-sum (sum payoffs))
  (define-values (accumulated _)
    (for/fold ([accumulated : [Listof Real] (list)] [init : Real 0]) ([y : Real (in-list payoffs)])
      (define next-init : Real (+ init (/ y payoff-sum)))
      (values ({inst cons Real [Listof Real]} next-init accumulated) next-init)))
  (reverse accumulated))
