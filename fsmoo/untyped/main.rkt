#lang racket
(random-seed 7480)

;; Run a Simulation of Interacting Automata

;; =============================================================================
(require "population.rkt" "utilities.rkt")

;; -> Void
;; effect: run timed simulation, create and display plot of average payoffs
;; effect: measure time needed for the simulation
(define (main)
   (simulation->lines
    (evolve (build-random-population 100) 1000 10 20))
   (void))

;; [Listof Payoff] -> [Listof [List Real Real]]
;; turn average payoffs into a list of Cartesian points 
(define (simulation->lines data)
  (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))

;; Population N N N -> [Listof Payoff]
;; computes the list of average payoffs over the evolution of population p for
;; c cycles of of match-ups with r rounds per match and at birth/death rate of s
(define (evolve p c s r)
  (cond
    [(zero? c) '()]
    [else (define p2 (send p match-up* r))
          (define pp (send p2 payoffs))
          (define p3 (send p2 death-birth s))
          (cons (relative-average pp r) (evolve p3 (- c 1) s r))]))

(time (main))
