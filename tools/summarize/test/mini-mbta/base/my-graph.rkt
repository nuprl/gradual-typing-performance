#lang racket

;; this module fixes a design mistake in the graph library
;; (don't define a syntactic abstraction when a function will do) 
;; (don't merge naming functionality with value generation capability; only as a convenience)

(provide 
 (all-from-out "./graph/graph/main.rkt")
 ;; formulate contract: 
 ;; if #:init is supplied, #:for-each may not be supplied 
 ;; if #:init is not supplied, #:for-each may be supplied 
 attach-edge-property)

;; ===================================================================================================
(require "./graph/graph/main.rkt")
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
