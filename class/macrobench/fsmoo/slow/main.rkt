#lang typed/racket
(random-seed 7480)

;; Run a Simulation of Interacting Automata
;; Run a Simulation of Interacting Automata

;; =============================================================================
(require
  "automata-adapted.rkt"
  "population-adapted.rkt"
)
(require/typed "utilities.rkt"
 (relative-average (-> [Listof Real] Real Real))
)

;; effect: run timed simulation, create and display plot of average payoffs
;; effect: measure time needed for the simulation
(define (main)
   (simulation->lines
    (evolve (build-random-population 100) 1000 10 20))
   (void))

(: simulation->lines (-> [Listof Payoff] (Listof (List Integer Real))))
;; turn average payoffs into a list of Cartesian points 
(define (simulation->lines data)
    (for/list : [Listof [List Integer Real]]
      ([d : Payoff (in-list data)][n : Integer (in-naturals)])
      (list n d)))

(: evolve (-> oPopulation Natural Natural Natural [Listof Payoff]))
;; computes the list of average payoffs over the evolution of population p for
;; c cycles of of match-ups with r rounds per match and at birth/death rate of s
(define (evolve p c s r)
  (cond
    [(zero? c) '()]
    [else (define p2 (send p match-up* r))
          ;; Note: r is typed as State even though State is not exported 
          (define pp (send p2 payoffs))
          (define p3 (send p2 death-birth s))
          ;; Note: s same as r
          ({inst cons Payoff [Listof Payoff]}
           (cast (relative-average pp r) Payoff)
           ;; Note: evolve is assigned (-> ... [Listof Probability])
           ;; even though it is explicitly typed ... [Listof Payoff]
           (evolve p3 (- c 1) s r))]))

(time (main))
