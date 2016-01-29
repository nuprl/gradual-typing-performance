#lang typed/racket/base

(provide
  Population
  build-population
  match-ups
  death-birth
)

(require benchmark-util)
(require/typed/check "population.rkt"
  [build-population (All (a) (-> Natural [-> Natural a] [Population a]))]
  [match-ups (All (a) (-> [Population a] Natural [-> a a (values Real Real a a)] [Listof Real]))]
  [death-birth (All (a) (-> [Population a] [Listof Real] Natural [Population a]))])


(define-type [Population a] (cons (Vectorof a) (Vectorof a)))
