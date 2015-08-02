#lang racket/base

;; Parse data from TiKZ lines
;; These functions are stolen from the `paper/script` folder of this repo

(provide
 (struct-out texnode)
 (struct-out texedge)
 string->texnode
 string->texedge
)

;; -----------------------------------------------------------------------------

(require
  racket/match
)

;; =============================================================================

(struct texnode (
  id ; Index
  index ; Index
  name ; String
) #:transparent)
;; A `texedge` is a (Pairof Index Index)
(struct texedge (
  from ; Index
  to ; Index
) #:transparent)

(define-syntax-rule (parse-error msg arg* ...)
  (error 'modulegraph (format msg arg* ...)))

;; For parsing nodes:
;;   \node (ID) [pos]? {\rkt{ID}{NAME}};
(define NODE_REGEXP
  #rx"^\\\\node *\\(([0-9]+)\\) *(\\[.*\\])? *\\{\\\\rkt\\{([0-9]+)\\}\\{(.+)\\}\\};$")
;; For parsing edges
;;   \draw[style]? (ID) edge (ID);
(define EDGE_REGEXP
  #rx"^\\\\draw\\[.*\\]? *\\(([0-9]+)\\)[^(]*\\(([0-9]+)\\);$")

;; Parsing
;(: string->index (-> String Index))
(define (string->index str)
  (string->number str))

;; Check if a line represents a real node, or is just for positioning
;; Parse a string into a texnode struct.
;(: string->texnode (-> String texnode))
(define (string->texnode str)
  (define m (regexp-match NODE_REGEXP str))
  (match m
    [(list _ id _ index name)
     #:when (and id index name)
     (texnode (or (string->index id) (parse-error "Could not parse integer from node id '~a'" id))
              (or (string->index index) (parse-error "Could not parse integer from node index '~a'" index))
              name)]
    [else
     #f]))

;; Parse a string into a tex edge.
;; Edges are represented as cons pairs of their source and destination.
;; Both source and dest. are represented as indexes.
;(: string->texedge (-> String texedge))
(define (string->texedge str)
  (define m (regexp-match EDGE_REGEXP str))
  (match m
    [(list _ id-dst id-src)
     #:when (and id-src id-dst)
     (texedge
      (string->index id-src)
      (string->index id-dst))]
    [else
     (parse-error "Cannot parse edge declaration '~a'" str)]))
