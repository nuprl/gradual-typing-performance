#lang racket

;; this module fixes a design mistake in the graph library
;; (don't define a syntactic abstraction when a function will do) 
;; (don't merge naming functionality with value generation capability; only as a convenience)

(provide 
 ;; formulate contract: 
 ;; if #:init is supplied, #:for-each may not be supplied 
 ;; if #:init is not supplied, #:for-each may be supplied 
 attach-edge-property)

(require
 (for-syntax racket/base racket/syntax syntax/parse 
             syntax/parse/experimental/template)
 racket/stxparam
 "graph-struct.rkt")

(define-syntax-parameter $from (syntax-rules ()))
(define-syntax-parameter $to (syntax-rules ()))

;; ===================================================================================================

(define (attach-edge-property graph #:init (x the-unsupplied-arg) #:for-each (f the-unsupplied-arg))
  (cond
    [(and (unsupplied-arg? x) (unsupplied-arg? f))
     (define-edge-property graph connection-on)
     (values connection-on connection-on->hash connection-on-set!)]
    [(unsupplied-arg? f)
     (define-edge-property graph connection-on #:init x)
     (values connection-on connection-on->hash connection-on-set!)]
    [(unsupplied-arg? x)
     (define-edge-property graph connection-on #:for-each f)
     (values connection-on connection-on->hash connection-on-set!)]))

(define-syntax (define-edge-property stx)
  (syntax-parse stx
    [(_ g prop (~or (~optional (~seq #:init init-val:expr))
                    (~optional (~seq #:for-each init-expr:expr ...))))
     #:with hash-name (generate-temporary #'prop)
     #:with prop-set! (format-id #'prop "~a-set!" #'prop)
     #:with prop->hash (format-id #'prop "~a->hash" #'prop)
     (template
      (begin
        (define hash-name (make-hash))
        (define (prop u v #:default 
                      [fail (λ () (error 'prop "no ~a value for edge ~a-~a" 
                                         'prop u v))])
          (hash-ref hash-name (list u v) fail))
        (define (prop->hash) hash-name)
        (define (prop-set! u v val) (hash-set! hash-name (list u v) val))
        (?? (let ([vs ((unweighted-graph-get-vertices g))])
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
