#lang typed/racket/base

;; this module fixes a design mistake in the graph library
;; (don't define a syntactic abstraction when a function will do) 
;; (don't merge naming functionality with value generation capability; only as a convenience)

(provide 
 ;; formulate contract: 
 ;; if #:init is supplied, #:for-each may not be supplied 
 ;; if #:init is not supplied, #:for-each may be supplied 
 attach-edge-property)

(require
 benchmark-util
 (for-syntax typed/racket/base racket/syntax syntax/parse 
             syntax/parse/experimental/template)
 racket/stxparam
 "graph-types.rkt"
 )

(define-syntax-parameter $from (syntax-rules ()))
(define-syntax-parameter $to (syntax-rules ()))

;; ===================================================================================================

;; 2015-04-18: "the-unsupplied-arg" is not part of typed/racket
(: attach-edge-property
                (->* (unweighted-graph #:init [Setof Line]) (#:for-each (U 'unsupplied-arg Any)) 
                     (Values 
                      (-> Station Station (Setof Line))
                      Any
                      (-> Station Station [Setof Line] Void))))
(define (attach-edge-property graph #:init x #:for-each (f 'unsupplied-arg))
  (cond
    [(and (eq? 'unsupplied-arg x) (eq? 'unsupplied-arg f))
     (define-edge-property graph connection-on)
     (values connection-on connection-on->hash connection-on-set!)]
    [(and (not (eq? 'unsupplied-arg x)) (eq? 'unsupplied-arg f))
     (define-edge-property graph connection-on #:init x)
     (values connection-on connection-on->hash connection-on-set!)]
    [(eq? 'unsupplied-arg x)
     (define-edge-property graph connection-on #:for-each f)
     (values connection-on connection-on->hash connection-on-set!)]
    [else (error "nope")]))

(define-syntax (define-edge-property stx)
  (syntax-parse stx
    [(_ g prop (~or (~optional (~seq #:init init-val:expr))
                    (~optional (~seq #:for-each init-expr:expr ...))))
     #:with hash-name (generate-temporary #'prop)
     #:with prop-set! (format-id #'prop "~a-set!" #'prop)
     #:with prop->hash (format-id #'prop "~a->hash" #'prop)
     (template
      (begin
        (: hash-name (HashTable (List String String) (Setof Line)))
        (define hash-name (make-hash))
        (: prop (-> String String (Setof Line)))
        (define (prop u v)
          (define (fail) (error 'prop "no value for edge"))
          ((inst hash-ref (List String String) (Setof Line) (Setof Line))
           hash-name (list u v) fail))
        (define (prop->hash) hash-name)
        (: prop-set! (-> String String (Setof Line) Void))
        (define (prop-set! u v val)
          ((inst hash-set! (List String String) (Setof Line))
           hash-name (list u v) val))
        (?? (let ([vs : (Listof String) ((unweighted-graph-get-vertices g))])
              (for* ([i vs] [j vs])
                (prop-set! i j 
                  (syntax-parameterize ([$from (syntax-id-rules () [_ i])]
                                        [$to (syntax-id-rules () [_ j])])
                    init-val)))))
        (?? (let ([vs ((unweighted-graph-get-vertices g))])
              (for* ([i vs] [j vs])
                (syntax-parameterize ([$from (syntax-id-rules () [_ i])]
                                      [$to (syntax-id-rules () [_ j])])
                  init-expr ...))))))]))
