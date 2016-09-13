#lang typed/racket
(random-seed 7480)

;; Run a Simulation of Interacting Automata
;; Run a Simulation of Interacting Automata

;; =============================================================================
(require
  "automata.rkt"
  "population.rkt"
)
(require "utilities.rkt"
)
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
  (let evolve ([c : Natural c] [s : Natural s] [r : Natural r])
  (cond
    [(zero? c) '()]
    [else (send p match-up* r)
          ;; Note: r is typed as State even though State is not exported 
          (define pp (send p payoffs))
          (send p death-birth s)
          ;; Note: s same as r
          ({inst cons Payoff [Listof Payoff]}
           (cast (relative-average pp r) Payoff)
           ;; Note: evolve is assigned (-> ... [Listof Probability])
           ;; even though it is explicitly typed ... [Listof Payoff]
           (evolve (- c 1) s r))])))

(time (main))
