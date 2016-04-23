#lang racket

(require "predict.rkt"
         "modulegraph.rkt"
         math/statistics)

;; (: raw-data->deltas :
;;    ModuleGraph
;;    Number
;;    (HashTable PredictionConfig (Listof Real))
;;    ->
;;    (HashTable PredictionConfig Real))
(define (raw-data->deltas mg unty raw)
  (for/hash ([(k v) (in-hash raw)])
    (define m (mean v))
    (values k (m . - . unty))))


