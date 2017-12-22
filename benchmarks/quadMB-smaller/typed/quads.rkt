#lang typed/racket/base

(provide
  group-quad-list
  block
  quad-name
  quad-attrs
  quad-list
  make-quadattrs
  box
  page-break
  column-break
  block-break
  page
  quad-attr-ref
  column
  quad-has-attr?
  word-string
  spacer
  line
  whitespace/nbsp?
  whitespace?
  quads->doc
  quads->column
  quad-car
  quads->page
  piece
  word-break
  optical-kern
  word
  quads->line
  quad->string
  quads->block
  ;; -- from quad-types.rkt
  page-break?
  column-break?
  block-break?
  optical-kern?
  spacer?
  run?
  line?
  word?
  word-break?
 )

;; -----------------------------------------------------------------------------

(require
 require-typed-check
 (only-in racket/string string-append*)
 "../base/lib-typed.rkt"
 "../base/quad-types.rkt"
 "../base/core-types.rkt")

;; =============================================================================

(: quad-name ((U GroupQuad Quad) -> QuadName))
(define (quad-name q)
  (car q))

(: quad-attrs ((U GroupQuad Quad) -> QuadAttrs))
(define (quad-attrs q)
  (car (cdr q)))

(: make-quadattr (QuadAttrKey QuadAttrValue -> QuadAttr))
(define (make-quadattr k v)
  (cons k v))

(: quadattr-value (QuadAttr -> QuadAttrValue))
(define (quadattr-value qa)
  (cdr qa))

(: quad-attr-keys (QuadAttrs -> (Listof QuadAttrKey)))
(define (quad-attr-keys qas)
  (if (empty? qas)
      qas
      ((inst map QuadAttrKey QuadAttr) car qas)))

(: quad-list (Quad -> QuadList))
(define (quad-list q)
  (cdr (cdr q)))

;; Because quad-list case-lam cannot be converted to a contract; 2 arity-1 cases
(: group-quad-list (GroupQuad -> GroupQuadList))
(define (group-quad-list q)
  (cdr (cdr q)))

(: quad-attr-ref (((U Quad QuadAttrs) QuadAttrKey) (QuadAttrValue) . ->* . QuadAttrValue))
(define (quad-attr-ref q-or-qas key [default attr-missing])
  (define qas (if (quad? q-or-qas) (quad-attrs q-or-qas) q-or-qas))
  (define qa-result (memf (λ([qap : QuadAttr]) (equal? key (car qap))) qas))
  (if qa-result
      ;; car beacause result of memf is a list tail; cadr because second element in pair
      (quadattr-value (car qa-result))
      (if (not (equal? default attr-missing)) default (error 'quad-attr-ref (format "Key ~v not found in quad attributes ~v" key qas)))))

(define cannot-be-common-attrs '(width x y page))
(define attr-missing (gensym))

(: quad->string (Quad -> String))
(define (quad->string x)
  (let loop : String ([x : (U Quad String) x])
    (cond
      [(string? x) x]
      ;; else branch relies on fact that x is either Quad or String
      [else (string-append* ((inst map String QuadListItem) loop (quad-list x)))])))

(: gather-common-attrs  ((Listof Quad) -> QuadAttrs))
(define (gather-common-attrs qs)
  (if (null? qs)
      qs
      (let loop
        ([qs qs]
         ;; start with the set of pairs in the first quad, then filter it down
         [candidate-attr-pairs : (Listof QuadAttr) (let ([first-attrs (quad-attrs (car qs))])
                                                     (if first-attrs
                                                         (for/fold ([caps : QuadAttrs null]) ([cap (in-list first-attrs)])
                                                           (if (member (car cap) cannot-be-common-attrs)
                                                               caps
                                                               (cons cap caps)))
                                                         null))])
        (cond
          [(null? candidate-attr-pairs) null] ; ran out of possible pairs, so return #f
          [(null? qs) candidate-attr-pairs] ; ran out of quads, so return common-attr-pairs
          ;; todo: reconsider type interface between output of this function and input to quadattrs
          [else (loop (cdr qs) (filter (λ([cap : QuadAttr]) (member cap (quad-attrs (car qs)))) candidate-attr-pairs))]))))

(: make-quadattrs (-> (Listof Any) QuadAttrs))
(define (make-quadattrs xs)
  ;; no point typing the input as (U QuadAttrKey QuadAttrValue)
  ;; because QuadAttrValue is Any, so that's the same as plain Any
  (let-values ([(ks vs even?) (for/fold
                               ([ks : (Listof QuadAttrKey) null][vs : (Listof QuadAttrValue) null][even? : Boolean #t])
                               ([x (in-list xs)])
                                (if (and even? (QuadAttrKey? x))
                                    (values (cons x ks) vs #f)
                                    (values ks (cons (assert x QuadAttrValue?) vs) #t)))])
    (when (not even?) (error 'quadattrs "odd number of elements in ~a" xs))
    ;; use for/fold rather than for/list to impliedly reverse the list
    ;; (having been reversed once above, this puts it back in order)
    (for/fold ([qas : QuadAttrs null])([k (in-list ks)][v (in-list vs)])
      (cons (make-quadattr k v) qas))))

(: whitespace? ((Any) (Boolean) . ->* . Boolean))
(define (whitespace? x [nbsp? #f])
  (cond
    [(quad? x) (whitespace? (quad-list x) nbsp?)]
    [(string? x) (or (and (regexp-match #px"\\p{Zs}" x) ; Zs = unicode whitespace category
                          (or nbsp? (not (regexp-match #px"\u00a0" x)))))] ; 00a0: nbsp
    [(list? x) (and (not (empty? x)) (andmap (λ(x) (whitespace? x nbsp?)) x))] ; andmap returns #t for empty lists
    [else #f]))

(define (whitespace/nbsp? x)
  (whitespace? x #t))

(: quad-car (-> Quad QuadListItem))
(define (quad-car q)
  (define ql (quad-list q))
  (if (not (empty? ql))
      ((inst car QuadListItem QuadList) ql)
      (error 'quad-car "quad-list empty")))

(: quad-has-attr? (Quad QuadAttrKey -> Boolean))
(define (quad-has-attr? q key)
  (and (member key (quad-attr-keys (quad-attrs q))) #t))

;; -----------------------------------------------------------------------------

(: box (->* ((U QuadAttrs HashableList))()  #:rest QuadListItem BoxQuad))
(define (box attrs . xs)
  (quad 'box (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

(: spacer (->* ((U QuadAttrs HashableList))()  #:rest QuadListItem SpacerQuad))
(define (spacer attrs . xs)
  (quad 'spacer (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

(: doc (->* ((U QuadAttrs HashableList))()  #:rest QuadListItem DocQuad))
(define (doc attrs . xs)
  (quad 'doc (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))
(: quads->doc (-> (Listof Quad) DocQuad))
(define (quads->doc qs)
  (apply doc (gather-common-attrs qs) qs))

(: optical-kern (->* ((U QuadAttrs HashableList)) () #:rest QuadListItem Optical-KernQuad))
(define (optical-kern attrs . xs)
  (quad 'optical-kern (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

(: piece (->* ((U QuadAttrs HashableList)) () #:rest GroupQuadListItem PieceQuad))
(define (piece attrs . xs)
  (quad 'piece (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

(: word (->* ((U QuadAttrs HashableList)) () #:rest QuadListItem WordQuad))
(define (word attrs . xs)
  (quad 'word (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

(: word-break (->* ((U QuadAttrs HashableList)) () #:rest QuadListItem Word-BreakQuad))
(define (word-break attrs . xs)
  (quad 'word-break (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

(: word-string (Quad -> String))
(define (word-string c)
  (define ql (quad-list c))
  (if (and (not (null? ql)) (string? (car ql)))
      (car ql)
      ""))

;;bg: first argument should be optional, but type error
(: page (->* ((U QuadAttrs HashableList)) () #:rest GroupQuadListItem PageQuad))
(define (page attrs . xs)
  (quad 'page (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

(: page-break (-> Page-BreakQuad))
(define (page-break)
  (define attrs '()) (define xs '())
  (quad 'page-break (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

(: quads->page (-> (Listof Quad) PageQuad))
(define (quads->page qs)
  (apply page (gather-common-attrs qs) qs))

(: column (->* ((U QuadAttrs HashableList)) () #:rest GroupQuadListItem ColumnQuad))
(define (column attrs . xs)
  (quad 'column (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))
(: quads->column (-> (Listof Quad) ColumnQuad))
(define (quads->column qs)
  (apply column (gather-common-attrs qs) qs))

(: column-break (-> Column-BreakQuad))
(define (column-break)
  (define attrs '()) (define xs '())
  (quad 'column-break (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

;;bg: first argument should be optional, but type error
(: line (->* ((U QuadAttrs HashableList)) () #:rest GroupQuadListItem LineQuad))
(define (line attrs . xs)
  (quad 'line (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))
(: quads->line (-> (Listof Quad) LineQuad))
(define (quads->line qs)
  (apply line (gather-common-attrs qs) qs))

(: block (->* ((U QuadAttrs HashableList)) () #:rest QuadListItem BlockQuad))
(define (block attrs . xs)
  (quad 'block (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))
(: quads->block (-> (Listof Quad) BlockQuad))
(define (quads->block qs)
  (apply block (gather-common-attrs qs) qs))

(: block-break (->* ((U HashableList QuadAttrs)) () #:rest QuadListItem Block-BreakQuad))
(define (block-break attrs . xs)
  (quad 'block-break (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))
