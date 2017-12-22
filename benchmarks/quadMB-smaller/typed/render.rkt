#lang typed/racket/base

(provide pdf-renderer%)

;; -----------------------------------------------------------------------------

(require
 require-typed-check
 "../base/core-types.rkt"
 "../base/quad-types.rkt"
 (only-in racket/list filter-not)
 (only-in typed/racket/draw Font% make-font current-ps-setup pdf-dc% the-color-database)
 (only-in typed/racket/class inherit define/override send* class new super-new send define/public object% this)
 (only-in racket/file display-to-file))
(require/typed/check "world.rkt"
  [world:font-size-key QuadAttrKey]
  [world:font-size-default (Parameterof Positive-Flonum)]
  [world:font-color-key QuadAttrKey]
  [world:font-color-default (Parameterof String)]
  [world:font-background-key QuadAttrKey]
  [world:font-background-default (Parameterof String)]
  [world:font-name-key QuadAttrKey]
  [world:font-name-default (Parameterof Font-Name)]
  [world:font-weight-key QuadAttrKey]
  [world:font-weight-default (Parameterof Font-Weight)]
  [world:font-style-key QuadAttrKey]
  [world:font-style-default (Parameterof Font-Style)]
  [world:paper-height-default (Parameterof Float)]
  [world:paper-width-default (Parameterof Float)]
  [world:x-position-key Symbol]
  [world:y-position-key Symbol]
  [world:ascent-key Symbol]
  [world:quality-default (Parameterof Index)]
  [world:draft-quality Index]
  [world:page-key Symbol])
(require/typed/check "utils.rkt"
  [flatten-quad (Quad -> (Listof Quad))])
(require/typed/check "quads.rkt"
  [quad-attr-ref (->* ((U Quad QuadAttrs) QuadAttrKey) (QuadAttrValue) QuadAttrValue)]
  [word (->* ((U QuadAttrs HashableList)) () #:rest QuadListItem WordQuad)]
  (quad-car (-> Quad QuadListItem))
  [whitespace/nbsp? (-> Any Boolean)]
  [quad-name (-> Quad QuadName)])

;; =============================================================================

(define abstract-renderer%

  (class object%
    (super-new)

    (define renderable-quads '(word box))

    ;; hash implementation
    (: render (Quad -> Any))
    (define/public (render doc-quad)
      (finalize
       (let ([rendering-input (flatten-quad (setup doc-quad))])
         (define page-quad-hash ((inst make-hash Nonnegative-Integer (Listof Quad))))
         (for ([q (in-list rendering-input)])
           (when (member (quad-name q) renderable-quads)
             ((inst hash-update! Nonnegative-Integer (Listof Quad)) page-quad-hash (cast (quad-attr-ref q world:page-key) Nonnegative-Integer) (λ(v) ((inst cons Quad (Listof Quad)) q v)) (λ() (cast null (Listof Quad))))))
         (map (λ([k : Nonnegative-Integer]) (render-page ((inst hash-ref Nonnegative-Integer (Listof Quad) (Listof Quad)) page-quad-hash k))) (sort (hash-keys page-quad-hash) <)))))

    (: render-element (Quad -> Any))
    (define/public (render-element q)
      (cond
        [(word? q) (render-word q)]
        [else q]))

    (: setup (Quad -> Quad))
    (define/public (setup q) q)

    ;; use in lieu of 'abstract' definition
    (: render-page ((Listof Quad) -> Void))
    (define/public (render-page qs) (void))

    ;; use in lieu of 'abstract' definition
    (: render-word (Quad -> Any))
    (define/public (render-word x) (word '()))

    (: finalize (Any -> Any))
    (define/public (finalize x) x)))

(define-syntax-rule (map/send method xs)
  (map (λ([x : Quad]) (method x)) xs))

;; this is outside class def'n because if inside,
;; (define dc ...) can't see it and type it correctly.
;; there may be a better way, but for now this is OK
(: dc-output-port Output-Port)
(define dc-output-port (open-output-bytes))

(define pdf-renderer%
  (class abstract-renderer%
    (super-new)

    (send* (current-ps-setup) (set-margin 0 0) (set-scaling 1.0 1.0))

    (define dc (new pdf-dc% [interactive #f][use-paper-bbox #f][as-eps #f]
                    [output dc-output-port]
                    [width (world:paper-width-default)][height (world:paper-height-default)]))


    (define/override (setup tx)
      (send* dc
        (start-doc "boing")
        (set-pen "black" 1 'solid)
        (set-brush "black" 'transparent)) ; no fill by default
      tx)

    (inherit render-element)


    (define font-cache ((inst make-hash (List String Nonnegative-Flonum Font-Style Font-Weight) (Instance Font%)) '()))
    (: get-cached-font (String Nonnegative-Flonum Font-Style Font-Weight ->  (Instance Font%)))
    (define (get-cached-font font size style weight)
      (hash-ref! font-cache (list font size style weight) (λ () (make-font #:face font #:size size #:style style #:weight weight))))


    (define/override (render-word w)
      (define word-font (cast (quad-attr-ref w world:font-name-key (world:font-name-default)) String))
      (define word-size (cast (quad-attr-ref w world:font-size-key (world:font-size-default)) Nonnegative-Float))
      (define word-style (cast (quad-attr-ref w world:font-style-key (world:font-style-default)) Font-Style))
      (define word-weight (cast (quad-attr-ref w world:font-weight-key (world:font-weight-default)) Font-Weight))
      (define word-color (cast (quad-attr-ref w world:font-color-key (world:font-color-default)) String))
      (define word-background (cast (quad-attr-ref w world:font-background-key (world:font-background-default)) String))
      (send dc set-font (get-cached-font word-font word-size word-style word-weight))
      (define foreground-color (send the-color-database find-color word-color))
      (when foreground-color
        (send dc set-text-foreground foreground-color))
      (define background-color (send the-color-database find-color word-background))
      (if background-color ; all invalid color-string values will return #f
          (send* dc (set-text-mode 'solid) (set-text-background background-color))
          (send dc set-text-mode 'transparent))

      (define word-text (cast (quad-car w) String))
      (send dc draw-text word-text (cast (quad-attr-ref w world:x-position-key) Float)
            ;; we want to align by baseline rather than top of box
            ;; thus, subtract ascent from y to put baseline at the y coordinate
            (- (cast (quad-attr-ref w world:y-position-key) Float) (cast (quad-attr-ref w world:ascent-key 0) Float)) #t))

    (define/override (render-page elements)
      (send dc start-page)
      (map/send render-element (filter-not whitespace/nbsp? elements))
      (send dc end-page))

    (define/override (finalize xs)
      (send dc end-doc)
      (get-output-bytes dc-output-port))

    (: render-to-file (Quad Path-String -> Void))
    (define/public (render-to-file doc-quad path)
      (define result-bytes (send this render doc-quad))
      (display-to-file result-bytes path #:exists 'replace #:mode 'binary))
    ))
