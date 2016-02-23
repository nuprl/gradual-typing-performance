#lang typed/racket/base

(provide
  BoxQuad
  RunQuad
  SpacerQuad
  DocQuad
  Optical-KernQuad
  PieceQuad
  WordQuad
  Word-BreakQuad
  PageQuad
  Page-BreakQuad
  ColumnQuad
  Column-BreakQuad
  LineQuad
  BlockQuad
  Block-BreakQuad
  ;; --
  page-break?
  column-break?
  block-break?
  word-break?
  column?
  line?
  run?
  word?
  optical-kern?
  spacer?
)

;; -----------------------------------------------------------------------------

(require "../base/core-types.rkt")

;; =============================================================================

(define-type BoxQuad (List* 'box QuadAttrs QuadList))
(define-type RunQuad (List* 'run QuadAttrs QuadList))
(define-type SpacerQuad (List* 'spacer QuadAttrs QuadList))
(define-type DocQuad (List* 'doc QuadAttrs QuadList))
(define-type Optical-KernQuad (List* 'optical-kern QuadAttrs QuadList))
(define-type PieceQuad (List* 'piece QuadAttrs GroupQuadList))
(define-type WordQuad (List* 'word QuadAttrs QuadList))
(define-type Word-BreakQuad (List* 'word-break QuadAttrs QuadList))
(define-type PageQuad (List* 'page QuadAttrs GroupQuadList))
(define-type Page-BreakQuad (List* 'page-break QuadAttrs QuadList))
(define-type ColumnQuad (List* 'column QuadAttrs GroupQuadList))
(define-type Column-BreakQuad (List* 'column-break QuadAttrs QuadList))
(define-type LineQuad (List* 'line QuadAttrs GroupQuadList))
(define-type BlockQuad (List* 'block QuadAttrs QuadList))
(define-type Block-BreakQuad (List* 'block-break QuadAttrs QuadList))

(define-predicate page-break? Page-BreakQuad)
(define-predicate column-break? Column-BreakQuad)
(define-predicate block-break? Block-BreakQuad)
(define-predicate word-break? Word-BreakQuad)
(define-predicate column? ColumnQuad)
(define-predicate line? LineQuad)
(define-predicate word? WordQuad)
(define-predicate run? RunQuad)
(define-predicate spacer? SpacerQuad)
(define-predicate optical-kern? Optical-KernQuad)
