#lang typed/racket/base

(provide
  PageQuad PageQuad? ;;bg TODO
  DocQuad DocQuad?
  BlockQuad BlockQuad? block
  quad-name
  quad-attrs
  quad-list
  make-quadattrs
  box
  BoxQuad?
  page-break? page-break
  column-break? column-break
  block-break? block-break
  page
  quad-attr-ref
  ColumnQuad ColumnQuad?
  column
  LineQuad
  LineQuad?
  quad-has-attr?
  run?
  word?
  word-break?
  word-string
  spacer?
  spacer
  line
  whitespace/nbsp?
  quads->doc
  quads->column
  quad-car
  quads->page
  piece
  word-break
  Word-BreakQuad?
  whitespace?
  optical-kern?
  Optical-KernQuad
  optical-kern
  word
  quads->line
  quad->string
  PieceQuad
  PieceQuad?
  Word-BreakQuad
  Word-BreakQuad?
  quads->block
 )

;; -----------------------------------------------------------------------------

(require
 benchmark-util
 (only-in racket/string string-append*)
 ;(for-syntax
 ; typed/racket/base
 ; racket/syntax
 ; racket/string)
 "../base/lib-typed.rkt"
 "../base/core-types.rkt")

;; note to self: a require/typed function with proper typing
;; is faster than a generic function + type assertion at location of call
;(require/typed racket/list
;  [flatten ((Listof QuadAttr) -> QuadAttrs)])
;(require/typed racket/string
; [string-append* ((Listof String) -> String)])
;; bg: not sure about keeping these
;(require
; typed/sugar/debug
; typed/sugar/string
; typed/sugar/list
; typed/sugar/define)

;; =============================================================================

(: quad-name (Quad -> QuadName))
(define (quad-name q)
  (car q))

;(define-syntax-rule (even-members xs)
;  (for/list : (Listof Any) ([(x i) (in-indexed xs)] #:when (even? i))
;    x))

(: quad-attrs (Quad -> QuadAttrs))
(define (quad-attrs q)
  (car (cdr q)))

(: make-quadattr (QuadAttrKey QuadAttrValue -> QuadAttr))
(define (make-quadattr k v)
  (cons k v))

;(define/typed (quadattr-key qa)
;  (QuadAttr -> QuadAttrKey)
;  (car qa))

(: quadattr-value (QuadAttr -> QuadAttrValue))
(define (quadattr-value qa)
  (cdr qa))

(: quad-attr-keys (QuadAttrs -> (Listof QuadAttrKey)))
(define (quad-attr-keys qas)
  (if (empty? qas)
      qas
      ((inst map QuadAttrKey QuadAttr) car qas)))

(: quad-list  (case->
   (GroupQuad -> GroupQuadList)
   (Quad -> QuadList)))
(define (quad-list q)
  (cdr (cdr q)))


(: quad-attr-ref (((U Quad QuadAttrs) QuadAttrKey) (QuadAttrValue) . ->* . QuadAttrValue))
(define (quad-attr-ref q-or-qas key [default attr-missing])
  (define qas (if (quad? q-or-qas) (quad-attrs q-or-qas) q-or-qas))
  (define qa-result (memf (λ([qap : QuadAttr]) (equal? key (car qap))) qas))
  (if qa-result
      ;; car beacause result of memf is a list tail; cadr because second element in pair
      (quadattr-value (car qa-result))
      (if (not (equal? default attr-missing)) default (error 'quad-attr-ref (format "Key ~v not found in quad attributes ~v" key qas)))))

;
;(define-syntax (quad-attr-ref/parameter stx)
;  (syntax-case stx ()
;    [(_ q key)
;     (with-syntax ([world:key-default (format-id stx "~a-default" (string-trim (symbol->string (syntax->datum #'key)) "-key"))])
;       #'(quad-attr-ref q key (world:key-default)))]))


(define cannot-be-common-attrs '(width x y page))
(define attr-missing (gensym))

;(: quad-ends-with? (Quad String -> Boolean))
;(define (quad-ends-with? q str)
;  (cond
;    [(not (empty? (quad-list q)))
;     (define last-item (list-ref (quad-list q) (sub1 (length (quad-list q)))))
;     (cond
;       [(string? last-item) (ends-with? last-item str)]
;       [(quad? last-item) (quad-ends-with? last-item str)]
;       [else #f])]
;    [else #f]))
;
;(: quad-append (Quad QuadListItem -> Quad))
;(define (quad-append q new-item)
;  (quad (quad-name q) (quad-attrs q) (append (quad-list q) (list new-item))))


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

;(define-syntax (define-quad-type stx)
;  (syntax-case stx ()
;    [(_ id)
;     #'(define-quad-type id #f)]
;    [(_ id wants-group?)
;     (with-syntax ([id? (format-id #'id "~a?" #'id)]
;                   [IdQuad (format-id #'id "~aQuad" (string-titlecase (symbol->string (syntax->datum #'id))))]
;                   [IdQuad? (format-id #'id "~aQuad?" (string-titlecase (symbol->string (syntax->datum #'id))))]
;                   [quads->id (format-id #'id "quads->~a" #'id)])
;       #`(begin
;           ;; quad converter
;           (define/typed (quads->id qs)
;             ((Listof Quad) -> IdQuad)
;             (apply id (gather-common-attrs qs) qs))
;
;           (define-type IdQuad (List* 'id QuadAttrs #,(if (syntax->datum #'wants-group?)
;                                                          #'GroupQuadList
;                                                          #'QuadList)))
;           (define-predicate IdQuad? IdQuad)
;           (define id? IdQuad?)
;
;           (define/typed (id [attrs '()] #:zzz [zzz 0] . xs)
;             (() ((U QuadAttrs HashableList) #:zzz Zero) #:rest #,(if (syntax->datum #'wants-group?)
;                                                                      #'GroupQuadListItem
;                                                                      #'QuadListItem) . ->* . IdQuad)
;             (quad 'id (if (QuadAttrs? attrs)
;                           attrs
;                           (make-quadattrs attrs)) xs))))]))

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

;(define-syntax (define-break-type stx)
;  (syntax-case stx ()
;    [(_ id)
;     #'(define-break-type id #f)]
;    [(_ id wants-group?)
;     (with-syntax ([split-on-id-breaks (format-id #'id "split-on-~a-breaks" #'id)]
;                   [id-break (format-id #'id "~a-break" #'id)]
;                   [id-break? (format-id #'id "~a-break?" #'id)]
;                   [multi-id (format-id #'id "multi~a" #'id)]
;                   [multi-id? (format-id #'id "multi~a?" #'id)]
;                   [quads->multi-id (format-id #'id "quads->multi~a" #'id)])
;       #'(begin
;           (define-quad-type id wants-group?)
;           (define-quad-type id-break) ; break is not necessarily a group
;           (define-quad-type multi-id wants-group?) ; multi-id is always a group
;           ;; breaker
;           (: split-on-id-breaks ((Listof Quad) -> (Listof (Listof Quad))))
;           (define (split-on-id-breaks xs)
;             ;; omit leading & trailing whitespace, because they're superfluous next to a break
;             (map (λ([xs : (Listof Quad)]) (trimf xs whitespace?)) (filter-split xs id-break?)))))]))
;
;(define quad= equal?)

(: quad-car (-> Quad QuadListItem))
(define (quad-car q)
  (define ql (quad-list q))
  (if (not (empty? ql))
      ((inst car QuadListItem QuadList) ql)
      (error 'quad-car "quad-list empty")))

;(define/typed (quad-cdr q)
;  (Quad -> QuadList)
;  (define ql (quad-list q))
;  (if (not (empty? ql))
;      ((inst cdr QuadListItem QuadList) ql)
;      (error 'quad-car "quad-list empty")))

(: quad-has-attr? (Quad QuadAttrKey -> Boolean))
(define (quad-has-attr? q key)
  (and ((inst member QuadAttrKey) key (quad-attr-keys (quad-attrs q))) #t))

;(define-quad-type box)
;(: quads->box (-> (Listof Quad) BoxQuad))
;(define (quads->box qs)
;  (apply box (gather-common-attrs qs) qs))
(define-type BoxQuad (List* 'box QuadAttrs QuadList))
(define-predicate BoxQuad? BoxQuad)
(define box? BoxQuad?)
(: box (->* ((U QuadAttrs HashableList))()  #:rest QuadListItem BoxQuad))
(define (box attrs . xs)
  (quad 'box (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

;(define-quad-type run)
(define-type RunQuad (List* 'run QuadAttrs QuadList))
(define-predicate RunQuad? RunQuad)
(define run? RunQuad?)
;(: run

;(define-quad-type spacer)
(define-type SpacerQuad (List* 'spacer QuadAttrs QuadList))
(define-predicate spacer? SpacerQuad)
(: spacer (->* ((U QuadAttrs HashableList))()  #:rest QuadListItem SpacerQuad))
(define (spacer attrs . xs)
  (quad 'spacer (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

;(define-quad-type doc)
(define-type DocQuad (List* 'doc QuadAttrs QuadList))
(define-predicate DocQuad? DocQuad)
(: doc (->* ((U QuadAttrs HashableList))()  #:rest QuadListItem DocQuad))
(define (doc attrs . xs)
  (quad 'doc (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))
(: quads->doc (-> (Listof Quad) DocQuad))
(define (quads->doc qs)
  (apply doc (gather-common-attrs qs) qs))

;(define-quad-type kern)
;(define-quad-type optical-kern)
(define-type Optical-KernQuad (List* 'optical-kern QuadAttrs QuadList))
(define-predicate Optical-KernQuad? Optical-KernQuad)
(define optical-kern? Optical-KernQuad?)
(: optical-kern (->* ((U QuadAttrs HashableList)) () #:rest QuadListItem Optical-KernQuad))
(define (optical-kern attrs . xs)
  (quad 'optical-kern (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

;(define-quad-type flag)
;(define-quad-type input)
;(define-quad-type piece #t)
(define-type PieceQuad (List* 'piece QuadAttrs GroupQuadList))
(define-predicate PieceQuad? PieceQuad)
(define piece? PieceQuad?)
(: piece (->* ((U QuadAttrs HashableList)) () #:rest GroupQuadListItem PieceQuad))
(define (piece attrs . xs)
  (quad 'piece (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

;(define-break-type word)
(define-type WordQuad (List* 'word QuadAttrs QuadList))
(define-predicate word? WordQuad)
(: word (->* ((U QuadAttrs HashableList)) () #:rest QuadListItem WordQuad))
(define (word attrs . xs)
  (quad 'word (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))
(define-type Word-BreakQuad (List* 'word-break QuadAttrs QuadList))
(define-predicate Word-BreakQuad? Word-BreakQuad)
(define word-break? Word-BreakQuad?)
(: word-break (->* ((U QuadAttrs HashableList)) () #:rest QuadListItem Word-BreakQuad))
(define (word-break attrs . xs)
  (quad 'word-break (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

(: word-string (Quad -> String))
(define (word-string c)
  (define ql (quad-list c))
  (if (and (not (null? ql)) (string? (car ql)))
      (car ql)
      ""))

;(define-break-type page #t)
;(define-break-type column #t)
;(define-break-type block)
;(define-break-type line #t)

(define-type PageQuad (List* 'page QuadAttrs GroupQuadList))
(define-predicate PageQuad? PageQuad)
;;bg: first argument should be optional, but type error
(: page (->* ((U QuadAttrs HashableList)) () #:rest GroupQuadListItem PageQuad))
(define (page attrs . xs)
  (quad 'page (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))
(define-type Page-BreakQuad (List* 'page-break QuadAttrs QuadList))
(define-predicate  Page-BreakQuad? Page-BreakQuad)
(define page-break? Page-BreakQuad?)
(: page-break (-> Page-BreakQuad))
(define (page-break)
  (define attrs '()) (define xs '())
  (quad 'page-break (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

(: quads->page (-> (Listof Quad) PageQuad))
(define (quads->page qs)
  (apply page (gather-common-attrs qs) qs))

(define-type ColumnQuad (List* 'column QuadAttrs GroupQuadList))
(define-predicate ColumnQuad? ColumnQuad)
(: column (->* ((U QuadAttrs HashableList)) () #:rest GroupQuadListItem ColumnQuad))
(define (column attrs . xs)
  (quad 'column (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))
(: quads->column (-> (Listof Quad) ColumnQuad))
(define (quads->column qs)
  (apply column (gather-common-attrs qs) qs))

(define-type Column-BreakQuad (List* 'column-break QuadAttrs QuadList))
(define-predicate Column-BreakQuad? Column-BreakQuad)
(define column-break? Column-BreakQuad?)
(: column-break (-> Column-BreakQuad))
(define (column-break)
  (define attrs '()) (define xs '())
  (quad 'column-break (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))

(define-type LineQuad (List* 'line QuadAttrs GroupQuadList))
(define-predicate LineQuad? LineQuad)
;;bg: first argument should be optional, but type error
(: line (->* ((U QuadAttrs HashableList)) () #:rest GroupQuadListItem LineQuad))
(define (line attrs . xs)
  (quad 'line (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))
(: quads->line (-> (Listof Quad) LineQuad))
(define (quads->line qs)
  (apply line (gather-common-attrs qs) qs))

(define-type BlockQuad (List* 'block QuadAttrs QuadList))
(define-predicate BlockQuad? BlockQuad)
(: block (->* ((U QuadAttrs HashableList)) () #:rest QuadListItem BlockQuad))
(define (block attrs . xs)
  (quad 'block (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))
(: quads->block (-> (Listof Quad) BlockQuad))
(define (quads->block qs)
  (apply block (gather-common-attrs qs) qs))

(define-type Block-BreakQuad (List* 'block-break QuadAttrs QuadList))
(define-predicate block-break? Block-BreakQuad)
(: block-break (->* ((U HashableList QuadAttrs)) () #:rest QuadListItem Block-BreakQuad))
(define (block-break attrs . xs)
  (quad 'block-break (if (QuadAttrs? attrs) attrs (make-quadattrs attrs)) xs))
