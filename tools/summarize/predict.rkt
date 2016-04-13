#lang typed/racket

(require (prefix-in mg: "modulegraph.rkt")
         (only-in "modulegraph.rkt" ModuleGraph)
         racket/match
         math/statistics
         "bitstring.rkt")
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

(: parse-prediction-config : Any -> PredictionConfig)
(define (parse-prediction-config v)
  (match v
    [`#(struct:untyped-typed-config ,rer ,ree)
     (untyped-typed-config (cast rer Natural) (cast ree Natural))]
    [`#(struct:typed-untyped-config ,rer ,ree)
     (typed-untyped-config (cast rer Natural) (cast ree Natural))]
    [`#(struct:optimize-config ,i)
     (optimize-config (cast i Natural))]))

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

(: config->features : ModuleGraph Bitstring -> (Listof PredictionConfig))
(define (config->features mg bs)
  ;; edges
  (append
   (apply append
          (for/list : (Listof (Listof PredictionConfig))
              ([m1 (in-list (mg:module-names mg))])
            (for/list : (Listof PredictionConfig)
                      ([m2 (in-list (mg:requires mg m1))]
                       #:when (xor (bit-high? bs (mg:name->index mg m1))
                                   (bit-high? bs (mg:name->index mg m2))))
              (define n1 (mg:name->index mg m1))
              (define n2 (mg:name->index mg m2))
              (cond [(bit-high? bs n1)
                     (typed-untyped-config n1 n2)]
                    [else
                     (untyped-typed-config n1 n2)]))))
   ;; optimized
   (for/list : (Listof PredictionConfig)
             ([_m (in-list (mg:module-names mg))]
              [n (in-naturals)]             
              #:when (bit-high? bs (cast n Natural)))
     (optimize-config (cast n Natural)))))

(: predict :
   ModuleGraph
   Bitstring
   (HashTable PredictionConfig Real)
   Real
   ->
   Real)
(define (predict mg bs perfs unty)
  (+ unty
     (for/sum : Real
              ([pc (in-list (config->features mg bs))])
       ((hash-ref perfs pc) . - . unty))))
