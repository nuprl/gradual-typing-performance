#lang typed/racket/base

(provide
  typeset
)

;; ----------------------------------------------------------------------------

(require
  require-typed-check
  (only-in racket/list append* empty empty? split-at drop-right)
  typed/racket/class
  (only-in racket/sequence sequence->list)
  (only-in math/flonum fl+ fl fl>)
  "../base/core-types.rkt"
  "../base/quad-types.rkt"
)
(require/typed/check "quads.rkt"
  [quads->doc (-> (Listof Quad) DocQuad)]
  (quads->page (-> (Listof Quad) PageQuad))
  (quads->block (-> (Listof Quad) BlockQuad))
  [quad-attrs (Quad -> QuadAttrs)]
  [line (->* ((U QuadAttrs HashableList)) () #:rest GroupQuadListItem LineQuad)]
  [quad-car (-> Quad QuadListItem)]
  [quad-name (-> Quad QuadName)]
  [quad-attr-ref (->* ((U Quad QuadAttrs) QuadAttrKey) (QuadAttrValue) QuadAttrValue)]
  [group-quad-list (GroupQuad -> GroupQuadList)]
  [quad-list (Quad -> QuadList)]
  (quad-has-attr? (Quad QuadAttrKey -> Boolean))
  (quads->column (-> (Listof Quad) ColumnQuad))
  [page (->* ((U QuadAttrs HashableList)) () #:rest GroupQuadListItem PageQuad)]
  [column (->* ((U QuadAttrs HashableList)) () #:rest GroupQuadListItem ColumnQuad)]
)
(require/typed/check "wrap.rkt"
  (insert-spacers-in-line ((LineQuad) ((Option Symbol)) . ->* . LineQuad))
  [wrap-adaptive (->* ((Listof Quad)) (Float) (Listof LineQuad))]
  [wrap-best (->* ((Listof Quad)) (Float) (Listof LineQuad))]
  [wrap-first (->* ((Listof Quad)) (Float) (Listof LineQuad))]
  [fill (->* (LineQuad) ((Option Float)) LineQuad)]
  [add-horiz-positions (-> GroupQuad GroupQuad)])
(require/typed/check "world.rkt"
  [world:line-looseness-key Symbol]
  [world:allow-hyphenated-last-word-in-paragraph Boolean]
  [world:line-looseness-tolerance Float]
  [world:line-index-key Symbol]
  [world:measure-key QuadAttrKey]
  [world:use-hyphenation? Boolean]
  [world:max-quality Index]
  [world:total-lines-key Symbol]
  [world:draft-quality Index]
  [world:quality-key QuadAttrKey]
  [world:quality-key-default (Parameterof Index)]
  [world:paper-width-default (Parameterof Float)]
  [world:column-count-key QuadAttrKey]
  [world:column-count-key-default (Parameterof Index)]
  [world:column-gutter-key QuadAttrKey]
  [world:column-gutter-key-default (Parameterof Float)]
  [world:column-index-key QuadAttrKey]
  [world:min-first-lines Index]
  [world:min-last-lines Index]
  [world:minimum-lines-per-column Index]
  [world:default-lines-per-column Index])
(require/typed/check "measure.rkt"
  [round-float (-> Float Float)]
  [load-text-cache-file (-> Void)]
  [update-text-cache-file (-> Void)]
)
(require/typed/check "utils.rkt"
  (merge-attrs (JoinableType * -> QuadAttrs))
  (split-last (All (A) ((Listof A) -> (values (Listof A) A))))
  (join-quads ((Listof Quad) -> (Listof Quad)))
  (hyphenate-quad (QuadListItem -> QuadListItem))
  (quad-map ((QuadListItem -> QuadListItem) Quad -> Quad))
  (group-quad-attr-set*
   (GroupQuad HashableList -> GroupQuad))
  (quad-attr-set*
   (Quad HashableList -> Quad))
  [attr-change (-> QuadAttrs HashableList QuadAttrs)]
  [compute-line-height (-> Quad Quad)]
  [add-vert-positions (-> GroupQuad GroupQuad)]
  [split-quad (-> Quad (Listof Quad))])
(require/typed/check "sugar-list.rkt"
 (slice-at (All (A) (case-> ((Listof A) Positive-Integer -> (Listof (Listof A)))
                   ((Listof A) Positive-Integer Boolean -> (Listof (Listof A)))))))
;; bg: should maybe import this
(require/typed "../base/csp/csp.rkt"
  [problem%  (Class (init-field [solver Any])
  (field [_solver Any])
  (field [_variable-domains Any])
  (field [_constraints Any])
  [reset (-> Void)]
  [custom-print (Output-Port Integer -> Void)]
  [custom-display (Output-Port -> Void)]
  [custom-write (Output-Port -> Void)]
  [add-variable (Any (Listof Any) . -> . Void)]
  [add-variables ((Listof Any) Any . -> . Void)]
  [add-constraint ((Index . -> . Boolean) (Listof Any) . -> . Void)][get-solution (-> HashTableTop)]
  [get-solutions (-> (Listof (HashTable String Integer)))]
  [get-solution-iter (-> HashTableTop)]
  [set-solver (Any . -> . Void)]
  [get-solver (-> Any)])])

;; =============================================================================

(define-type Block-Type (Listof Quad))
(define-type Multicolumn-Type (Listof Block-Type))
(define-type Multipage-Type (Listof Multicolumn-Type))

(: typeset (-> Quad DocQuad))
(define (typeset x)
  (load-text-cache-file)
  (define pages (append*
                 (for/list : (Listof (Listof PageQuad))
                   ([multipage (in-list (input->nested-blocks x))])
                   (columns->pages (append*
                                    (for/list : (Listof (Listof ColumnQuad))
                                      ([multicolumn (in-list multipage)])
                                      (lines->columns (append*
                                                       (for/list : (Listof (Listof LineQuad))
                                                         ([block-quads (in-list multicolumn)])
                                                         (block-quads->lines block-quads))))))))))
  (define doc (pages->doc pages))
  (update-text-cache-file)
  doc)

;; -----------------------------------------------------------------------------

(: cons-reverse (All (A B) ((Listof A) (Listof B) -> (Pairof (Listof A) (Listof B)))))
(define (cons-reverse xs ys)
  ((inst cons (Listof A) (Listof B)) ((inst reverse A) xs) ys))


(: input->nested-blocks (Quad . -> . (Listof Multipage-Type)))
(define (input->nested-blocks i)
  (define-values (mps mcs bs b)
    (for/fold ([multipages : (Listof Multipage-Type) empty]
               [multicolumns : (Listof Multicolumn-Type) empty]
               [blocks : (Listof Block-Type) empty]
               [block-acc : Block-Type empty])
              ([q (in-list (split-quad i))])
      (cond
        [(page-break? q) (values (cons-reverse (cons-reverse (cons-reverse block-acc blocks) multicolumns) multipages) empty empty empty)]
        [(column-break? q) (values multipages (cons-reverse (cons-reverse block-acc blocks) multicolumns) empty empty)]
        [(block-break? q) (values multipages multicolumns (cons-reverse block-acc blocks) empty)]
        [else (values multipages multicolumns blocks (cons q block-acc))])))
  (reverse (cons-reverse (cons-reverse ((inst cons-reverse Quad Block-Type) b bs) mcs) mps)))

(: merge-adjacent-within (Quad . -> . Quad))
(define (merge-adjacent-within q)
  (quad (quad-name q) (quad-attrs q) (join-quads (cast (quad-list q) (Listof Quad)))))

(: hyphenate-quad-except-last-word (Quad . -> . Quad))
(define (hyphenate-quad-except-last-word q)
  ;(log-quad-debug "last word will not be hyphenated")
  (define-values (first-quads last-quad) ((inst split-last QuadListItem) (quad-list q)))
  (quad (quad-name q) (quad-attrs q) (append ((inst map QuadListItem QuadListItem) hyphenate-quad first-quads) (list last-quad))))

(: average-looseness ((Listof Quad) . -> . Float))
(define (average-looseness lines)
  (if (<= (length lines) 1)
      0.0
      (let ([lines-to-measure (drop-right lines 1)]) ; exclude last line from looseness calculation
        (round-float (/ (foldl fl+ 0.0 ((inst map Float Quad) (λ(line) (cast (quad-attr-ref line world:line-looseness-key 0.0) Float)) lines-to-measure)) (- (fl (length lines)) 1.0))))))

;; todo: introduce a Quad subtype where quad-list is guaranteed to be all Quads (no strings)
(: block->lines (BlockQuad . -> . (Listof LineQuad)))
(define (block->lines b)
  (define quality (assert (quad-attr-ref b world:quality-key (world:quality-key-default)) Index?))
  (: wrap-quads ((Listof Quad) . -> . (Listof LineQuad)))
  (define (wrap-quads qs)
    (define wrap-proc (cond
                        [(>= quality world:max-quality) wrap-best]
                        [(<= quality world:draft-quality) wrap-first]
                        [else wrap-adaptive]))
    (wrap-proc qs))
  ;(log-quad-debug "wrapping lines")
  ;(log-quad-debug "quality = ~a" quality)
  ;(log-quad-debug "looseness tolerance = ~a" world:line-looseness-tolerance)
  (define wrapped-lines-without-hyphens (wrap-quads (cast (quad-list b) (Listof Quad)))) ; 100/150
  ;(log-quad-debug* (log-debug-lines wrapped-lines-without-hyphens))
  (define avg-looseness (average-looseness wrapped-lines-without-hyphens))
  (define gets-hyphenation? (and world:use-hyphenation?
                                 (fl> avg-looseness world:line-looseness-tolerance)))
  ;(log-quad-debug "average looseness = ~a" avg-looseness)
  ;(log-quad-debug (if gets-hyphenation? "hyphenating" "no hyphenation needed"))
  (define wrapped-lines (if gets-hyphenation?
                            (wrap-quads (split-quad (cast ((if world:allow-hyphenated-last-word-in-paragraph
                                                               hyphenate-quad
                                                               hyphenate-quad-except-last-word) (merge-adjacent-within b)) Quad)))
                            wrapped-lines-without-hyphens))
  ;(when gets-hyphenation? (log-quad-debug* (log-debug-lines wrapped-lines)))
  ;(log-quad-debug "final looseness = ~a" (average-looseness wrapped-lines))
  (map insert-spacers-in-line
       (for/list : (Listof LineQuad) ([line-idx (in-naturals)][the-line (in-list wrapped-lines)])
         (apply line (attr-change (quad-attrs the-line) (list 'line-idx line-idx 'lines (length wrapped-lines))) (group-quad-list the-line)))))


(: number-pages ((Listof PageQuad) . -> . (Listof PageQuad)))
(define (number-pages ps)
  (for/list ([i (in-naturals)][p (in-list ps)])
    (apply page (merge-attrs (quad-attrs p) `(page ,i)) (group-quad-list p))))

(: pages->doc ((Listof PageQuad) . -> . DocQuad))
(define (pages->doc ps)
  ;; todo: resolve xrefs and other last-minute tasks
  ;; todo: generalize computation of widths and heights, recursively
  (: columns-mapper (PageQuad . -> . PageQuad))
  (define (columns-mapper page-in)
    (apply page (quad-attrs page-in)
           (map add-vert-positions (for/list : (Listof ColumnQuad) ([col (in-list (quad-list page-in))])
             (assert col column?)
             (apply column (quad-attrs col) (map (λ([ln : Quad]) (assert ln line?) (compute-line-height (add-horiz-positions (fill ln)))) (group-quad-list col)))))))
  (define mapped-pages (map columns-mapper (number-pages ps)))
  (define doc (quads->doc mapped-pages))
  doc)


(: lines->columns ((Listof LineQuad) . -> . (Listof ColumnQuad)))
(define (lines->columns lines)
  (define prob (new problem% [solver #f]))
  (define max-column-lines world:default-lines-per-column)
  (define-values (columns ignored-return-value)
    (for/fold ([columns : (Listof ColumnQuad) empty][lines-remaining : (Listof LineQuad) lines])
              ([col-idx : Nonnegative-Integer (stop-before (in-naturals) (λ(x) (empty? lines-remaining)))])
      ;bg;(log-quad-info "making column ~a" (add1 col-idx))
      ;; domain constraint is best way to simplify csp, because it limits the search space.
      ;; search from largest possible value to smallest.
      ;; largest possible is the minimum of the max column lines, or
      ;; the number of lines left (modulo minimum page lines) ...
      (define viable-column-range
        (sequence->list (in-range (min max-column-lines (max
                                      (length lines-remaining)
                                      (- (length lines-remaining) world:minimum-lines-per-column)))
               ;; ... and the smallest possible is 1, or the current minimum lines.
               ;; (sub1 insures that range is inclusive of last value.)
               (sub1 (min 1 world:minimum-lines-per-column)) -1)))
      ;bg;(log-quad-debug "viable number of lines for this column to start =\n~a" viable-column-range)
      (send prob add-variable "column-lines" viable-column-range)
      ;; greediness constraint: leave enough lines for next page, or take all
      (: greediness-constraint (Index . -> . Boolean))
      (define (greediness-constraint pl)
        (define leftover (- (length lines-remaining) pl))
        (or (= leftover 0) (>= leftover world:minimum-lines-per-column)))
      (send prob add-constraint greediness-constraint '("column-lines"))
      ;(log-quad-debug "viable number of lines after greediness constraint =\n~a" ((inst map Integer (HashTable String Integer)) (λ(x) (hash-ref x "column-lines")) (send prob get-solutions)))
      ;; last lines constraint: don't take page that will end with too few lines of last paragraph.
      (: last-lines-constraint (-> Index Boolean))
      (define (last-lines-constraint pl)
        (define last-line-of-page ((inst list-ref Quad) lines-remaining (sub1 pl)))
        (define lines-in-this-paragraph (assert (quad-attr-ref last-line-of-page world:total-lines-key) Index?))
        (define line-index-of-last-line (assert (quad-attr-ref last-line-of-page world:line-index-key) Index?))
        (define (paragraph-too-short-to-meet-constraint?)
          (< lines-in-this-paragraph world:min-last-lines))
        (or (paragraph-too-short-to-meet-constraint?)
            (>= (add1 line-index-of-last-line) world:min-last-lines)))
      (send prob add-constraint last-lines-constraint '("column-lines"))
      ;(log-quad-debug "viable number of lines after last-lines constraint =\n~a" ((inst map Integer (HashTable String Integer)) (λ(x) (hash-ref x "column-lines")) (send prob get-solutions)))
      ;; first lines constraint: don't take page that will leave too few lines at top of next page
      (: first-lines-constraint (Index (Listof Quad) . -> . Boolean))
      (define (first-lines-constraint pl lines-remaining)
        (define last-line-of-page (list-ref lines-remaining (sub1 pl)))
        (define lines-in-this-paragraph (assert (quad-attr-ref last-line-of-page world:total-lines-key) integer?))
        (define line-index-of-last-line (assert (quad-attr-ref last-line-of-page world:line-index-key) integer?))
        (define lines-that-will-remain (- lines-in-this-paragraph (add1 line-index-of-last-line)))
        (define (paragraph-too-short-to-meet-constraint?)
          (< lines-in-this-paragraph world:min-first-lines))
        (or (paragraph-too-short-to-meet-constraint?)
            (= 0 lines-that-will-remain) ; ok to use all lines ...
            (>= lines-that-will-remain world:min-first-lines))) ; but if any remain, must be minimum number.
      (send prob add-constraint (λ(x) (first-lines-constraint (assert x Index?) lines-remaining)) '("column-lines"))
      ;(log-quad-debug "viable number of lines after first-lines constraint =\n~a" ((inst map Integer (HashTable String Integer)) (λ(x) (hash-ref x "column-lines")) (send prob get-solutions)))
      (define s (send prob get-solution))
      (define how-many-lines-to-take (assert (hash-ref s "column-lines") index?))
      (define-values (lines-to-take lines-to-leave) (split-at lines-remaining how-many-lines-to-take))
      ;(log-quad-debug "taking ~a lines for column ~a:" how-many-lines-to-take (add1 col-idx))
      ;(map (λ([idx : Index] [line : LineQuad]) (log-quad-debug "~a:~a ~v" (add1 col-idx) (add1 idx) (quad->string line))) (range how-many-lines-to-take) lines-to-take)
      (send prob reset)
      (define new-column (quads->column lines-to-take))
      (values (cons (apply column (attr-change (quad-attrs new-column) (list world:column-index-key col-idx)) (group-quad-list new-column)) columns) lines-to-leave)))
  (reverse columns))


(: columns->pages ((Listof ColumnQuad) . -> . (Listof PageQuad)))
(define (columns->pages cols)
  (define columns-per-page (cast (quad-attr-ref (car cols) world:column-count-key (world:column-count-key-default)) Positive-Integer))
  (define column-gutter (assert (quad-attr-ref (car cols) world:column-gutter-key (world:column-gutter-key-default)) flonum?))
  ;; don't use default value here. If the col doesn't have a measure key,
  ;; it deserves to be an error, because that means the line was composed incorrectly.
  (when (not (quad-has-attr? (car cols) world:measure-key))
    (error 'columns->pages "column attrs contain no measure key: ~a ~a" (quad-attrs (car cols)) (quad-car (car cols))))
  (define column-width (assert (quad-attr-ref (car cols) world:measure-key) flonum?))
  (define width-of-printed-area (+ (* columns-per-page column-width) (* (sub1 columns-per-page) column-gutter)))
  (define result-pages
    ((inst map PageQuad (Listof Quad)) (λ(cols) (quads->page cols))
                                       (for/list : (Listof (Listof Quad)) ([page-cols (in-list (slice-at cols columns-per-page))])
                                         (define-values (last-x cols)
                                           (for/fold ([current-x : Float (/ (- (world:paper-width-default) width-of-printed-area) 2.0)]
                                                      [cols : (Listof Quad) empty])
                                                     ([col (in-list page-cols)][idx (in-naturals)])
                                             (values (foldl fl+ 0.0 (list current-x column-width column-gutter)) (cons (quad-attr-set* col (list 'x current-x 'y 40.0 world:column-index-key idx)) cols))))
                                         (reverse cols))))
  result-pages)

(: block-quads->lines ((Listof Quad) . -> . (Listof LineQuad)))
(define (block-quads->lines qs)
  (block->lines (quads->block qs)))
