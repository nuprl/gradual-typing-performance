#lang typed/racket/base

(provide
 (all-defined-out)
 (all-from-out typed/racket/draw))

;; -----------------------------------------------------------------------------

(require
 (for-syntax typed/racket/base racket/syntax)
 (only-in typed/racket/draw Font-Weight Font-Style))

;; =============================================================================

(define-type QuadName Symbol)
(define-predicate QuadName? QuadName)
(define-type QuadAttrKey Symbol)
(define-predicate QuadAttrKey? QuadAttrKey)
(define-type QuadAttrValue (U Float Index String Symbol Boolean Quad QuadAttrs QuadList Integer))
(define-predicate QuadAttrValue? QuadAttrValue)

;; QuadAttr could be a list, but that would take twice as many cons cells.
;; try the economical approach.
(define-type QuadAttr (Pairof QuadAttrKey QuadAttrValue))
(define-predicate QuadAttr? QuadAttr)
(define-type QuadAttrs (Listof QuadAttr))
(define-predicate QuadAttrs? QuadAttrs)
(define quad-attrs? QuadAttrs?)
#|
;; mutually recursive version
(define-type HashableListKey (U Null (Pairof QuadAttrKey HashableListValue)))
(define-type HashableListValue (Pairof QuadAttrValue HashableListKey))
(define-type HashableList  HashableListKey)
(define-predicate HashableList? HashableList)
|#
(define-type HashableList  (Rec duo (U Null (List* QuadAttrKey Any duo))))
(define-predicate HashableList? HashableList)
(define-type JoinableType (U Quad QuadAttrs HashableList))


(define-type QuadListItem (U String Quad))
(define-predicate QuadListItem? QuadListItem)
(define-type QuadList (Listof QuadListItem))
(define-predicate QuadList? QuadList)
(define-type GroupQuadListItem Quad)
(define-predicate GroupQuadListItem? GroupQuadListItem)
(define-type GroupQuadList (Listof GroupQuadListItem))
(define-predicate GroupQuadList? GroupQuadList)
(define-type (Treeof A) (Rec as (U A (Listof as))))


;; funky implementation
(define-type Quad (List* QuadName QuadAttrs QuadList))
(define-predicate Quad? Quad)
(define-type GroupQuad (List* QuadName QuadAttrs GroupQuadList))
(define-predicate GroupQuad? GroupQuad)
(define-predicate quad? Quad)

;; quad wants to be generic
;; if it's a function, it must impose a type on its output value
;; whereas if it's syntax, it can avoid demanding or imposing any typing
;;bg worried I can't keep the syntax
(define-syntax-rule (quad name attrs items)
  (list* name attrs items))

(define-type QuadSet (List QuadName QuadAttrs (Listof Quad)))
(define-predicate QuadSet? QuadSet)


(define-type Font-Name String)
(define-predicate Font-Name? Font-Name)
(define-type Font-Size Positive-Flonum)
(define-predicate Font-Size? Font-Size)
(define-predicate Font-Weight? Font-Weight)
(define-predicate Font-Style? Font-Style)

(define-predicate Index? Index)

;; Index is arguably the stricter type for Breakpoint,
;; but in practice it's annoying because doing math with Indexes
;; often leads to non-Index values.
(define-type Breakpoint Nonnegative-Integer)
(define-predicate Breakpoint? Breakpoint)
