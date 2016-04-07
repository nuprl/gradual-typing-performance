#lang typed/racket/base

(require (prefix-in mg: gtp-summarize/modulegraph)
         (only-in gtp-summarize/modulegraph
                  ModuleGraph)
         racket/match)
(provide (all-defined-out))

(struct untyped-typed-config
  ([requirer : Natural]
   [requiree : Natural])
  #:transparent)
(struct typed-untyped-config
  ([requirer : Natural]
   [requiree : Natural])
  #:transparent)
(struct optimize-config
  ([module-index : Natural])
  #:transparent)
(define-type UntypedTypedConfig untyped-typed-config)
(define-type TypedUntypedConfig typed-untyped-config)
(define-type OptimizeConfig     optimize-config)
(define-type PredictionConfig (U UntypedTypedConfig
                                 TypedUntypedConfig
                                 OptimizeConfig))

(: prediction-configs : ModuleGraph -> (Listof PredictionConfig))
(define (prediction-configs mg)
  (append
   (apply append
          (for*/list : (Listof (Listof PredictionConfig))
            ([requirer : String (in-list (mg:module-names mg))]
             [requiree : String (in-list (mg:requires mg requirer))])
            (define requirer-n (mg:name->index mg requirer))
            (define requiree-n (mg:name->index mg requiree))
            (list (untyped-typed-config requirer-n requiree-n)
                  (typed-untyped-config requirer-n requiree-n))))
   (for/list : (Listof PredictionConfig)
             ([m-s (in-list (mg:module-names mg))])
     (optimize-config (mg:name->index mg m-s)))))

(: prediction-config->name : PredictionConfig -> String)
(define (prediction-config->name pc)
  (match pc
    [(untyped-typed-config rer ree) (format "edge-u~a-t~a" rer ree)]
    [(typed-untyped-config rer ree) (format "edge-t~a-u~a" rer ree)]
    [(optimize-config m) (format "optimize-~a" m)]))
