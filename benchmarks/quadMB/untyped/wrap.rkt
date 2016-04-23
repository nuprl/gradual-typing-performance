#lang racket/base

(provide
  add-horiz-positions
  fill
  wrap-best
  wrap-first
  wrap-adaptive
  insert-spacers-in-line
)

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  "../base/core.rkt"
  (only-in math/flonum fl+ fl= fl fl/ fl- fl> fl* fllog flabs flmax flexpt)
  (only-in racket/list empty empty? partition second first last split-at-right append-map drop-right drop)
  (only-in racket/vector vector-map vector-copy vector-count)
  (only-in math/statistics stddev) ;;bg: cause for alarm?
  (for-syntax
    racket/base
    (only-in racket/syntax format-id)
    (only-in racket/string string-trim)))
(require/check
  "ocm-struct.rkt"
  "penalty-struct.rkt"
(only-in "measure.rkt"
  measure-ascent
  measure-text
  round-float)
(only-in "quads.rkt"
  quads->line
  quad-attrs
  quad-name
  line?
  quad-attr-ref
  word?
  quad->string
  optical-kern
  optical-kern?
  word-break
  piece
  line
  whitespace/nbsp?
  whitespace?
  spacer?
  run?
  word-break?
  word-string
  quad-list
  group-quad-list
  spacer
  quad-has-attr?)
(only-in "world.rkt"
  world:last-line-can-be-short
  world:new-line-penalty
  world:hyphen-penalty
  world:hyphen-limit
  world:allowed-overfull-ratio
  world:line-looseness-key
  world:ascent-key
  world:optical-overhang
  world:hanging-chars
  world:use-optical-kerns?
  world:before-break-key
  world:default-word-break-list
  world:no-break-key
  world:word-break-key
  world:spaces
  world:empty-string
  world:hyphens-and-dashes
  world:soft-hyphen
  world:unbreakable-key
  world:minimum-last-line-chars
  world:measure-default
  world:measure-key
  world:font-size-key
  world:font-size-default
  world:font-name-key
  world:font-name-default
  world:font-weight-key
  world:font-weight-default
  world:font-style-key
  world:font-style-default
  world:line-index-key
  world:total-lines-key
  world:horiz-alignment-last-line-key
  world:horiz-alignment-key
  world:horiz-alignment-default
  world:width-key
  world:x-position-key
  world:y-position-key)
(only-in "utils.rkt"
 attr-change
 join-quads
 attr-delete
 split-last
 flatten-quadtree
 merge-attrs
 group-quad-attr-remove*
 quad-attr-remove*
 quad-attr-set)
(only-in "ocm.rkt"
  make-ocm
  ocm-min-index
  ocm-min-entry)
(only-in "sugar-list.rkt"
  shifts
 slicef-after
 break-at))

;; =============================================================================

;; bg: from quads.rkt
(define-syntax (quad-attr-ref/parameter stx)
  (syntax-case stx ()
    [(_ q key)
     (with-syntax ([world:key-default (format-id stx "~a-default" (string-trim (symbol->string (syntax->datum #'key)) "-key"))])
       #'(quad-attr-ref q key (world:key-default)))]))

;; -----------------------------------------------------------------------------

;; predicate for the soft hyphen
(define (soft-hyphen? x)
  (equal? (format "~a" world:soft-hyphen) x))

;; visible characters that also mark possible breakpoints
(define (visible-breakable? x)
  (and (member x world:hyphens-and-dashes) #t))

;; invisible characters that denote possible breakpoints
(define (invisible-breakable? x)
  (and (member x (cons world:empty-string world:spaces)) #t))

;; union of visible & invisible
(define (breakable? x)
  (cond
    [(string? x) (or (visible-breakable? x) (invisible-breakable? x))]
    ;; word? should have a filter that returns a Quad type, then the Quad? check will be unnecessary
    [(and (quad? x) (word? x)) (breakable? (word-string x))]
    [else #f]))

;; used by insert-spacers to determine which characters
;; can be surrounded by stretchy spacers
(define (takes-justification-space? x)
  (whitespace/nbsp? x))

;; test if a quad can be a word break:
;; either it's an explicit word break,
;; or it's breakable (and can be converted to a word break)
(define (possible-word-break-quad? q)
  (or (word-break? q) (breakable? q)))


;; convert a possible word break into an actual one
(define (convert-to-word-break q)
  (when (not (possible-word-break-quad? q))
    (error 'convert-to-word-break "input is not a possible word break:" q))
  (define result (cond
                   [(word-break? q) q]
                   [(word? q)
                    (define str (word-string q)) ; str will be one character long, because we've exploded our input
                    (apply word-break
                           (merge-attrs q ; take q's attributes for formatting purposes
                                        (cond
                                          ;; a space is ordinarily visible, but disappears at the end of a line
                                          [(equal? str " ") (list world:no-break-key " " world:before-break-key "")]
                                          ;; soft hyphen is ordinarily invisible, but appears at the end of a line
                                          [(soft-hyphen? str) (list world:no-break-key "" world:before-break-key "-")]
                                          ;; a visible breakable character is always visible
                                          [(visible-breakable? str) (list world:no-break-key str world:before-break-key str)]
                                          [else (world:default-word-break-list)])) (quad-list q))]
                   [else #f]))
  (or result (error 'convert-to-word-break "result was a not word break for input:" q)))

(define (make-unbreakable q)
  (quad-attr-set q world:unbreakable-key #t))


;; take list of atomic quads and gather them into pieces
;; a piece is an indivisible chunk of a line.
;; meaning, a line can wrap at a piece boundary, but not elsewhere.
;; hyphenation produces more, smaller pieces, which means more linebreak opportunities
;; but this also makes wrapping slower.
(define (make-pieces qs)
  (define-values (breakable-items items-to-make-unbreakable) (split-at-right qs (min world:minimum-last-line-chars (length qs))))
  (define unbreak-qs (append breakable-items (map make-unbreakable items-to-make-unbreakable)))
  (define lists-of-quads (slicef-after unbreak-qs (λ(q) (and (possible-word-break-quad? q) (not (quad-attr-ref q world:unbreakable-key #f))))))
  (define-values (first-lists-of-quads last-list-of-quads) (split-last lists-of-quads))
  (define (make-first-pieces qs)
    (let-values ([(first-qs last-q) (split-last qs)])
      (apply piece (list world:word-break-key (convert-to-word-break last-q)) first-qs)))
  (append (map make-first-pieces first-lists-of-quads)
          (list (apply piece null last-list-of-quads))))


;; extract font attributes from quad, or get default values
(define (font-attributes-with-defaults q)
  (list
   (let ([size (quad-attr-ref q world:font-size-key)])
             (if (exact-integer? size) (fl size) size))
   (quad-attr-ref/parameter q world:font-name-key)
   (quad-attr-ref/parameter q world:font-weight-key)
   (quad-attr-ref/parameter q world:font-style-key)))

;;; get the width of a quad.
;;; Try the attr first, and if it's not available, compute the width.
;;; comes in fast or slow versions.
;;; not designed to update the source quad.
(define (quad-width q)
  (cond
    [(quad-has-attr? q world:width-key) (fl (quad-attr-ref q world:width-key))]
    [(ormap (λ(pred) (pred q)) (list char? run? word? word-break?))
     (apply measure-text (word-string q)
            (font-attributes-with-defaults q))]
    [(line? q) (foldl fl+ 0.0 (map quad-width (group-quad-list q)))]
    [else 0.0]))

;; get the ascent (distance from top of text to baseline)
;; used by renderer to align text runs baseline-to-baseline.
;; consult the attrs, and if not available, compute it.
;; not designed to update the source quad.
(define (ascent q)
  (define ascent-value-or-false (quad-attr-ref q world:ascent-key #f))
  (if (and ascent-value-or-false (flonum? ascent-value-or-false))
      ascent-value-or-false
      (cond
        [(ormap (λ(pred) (pred q)) (list char? run? word? word-break?))
         (apply measure-ascent (word-string q) (font-attributes-with-defaults q))]
        [else 0.0])))


;; convert a piece into its final form, which depends on location.
;; if a piece appears at the end of a line, it is rendered in "before break" mode.
;;; if a piece appears elsewhere in a line, it is rendered in "no break" mode.
;;; this allows the appearance of a piece to change depending on whether it's at the end.
;;; and thus give correct behavior to trailing word spaces, soft hyphens, etc.

(define (render-piece p [before-break? #f])
  ;; a piece doesn't necessarily have a word-break item in it.
  ;; only needs it if the appearance of the piece changes based on location.
  ;; so words are likely to have a word-break item; boxes not.
  ;; the word break item contains the different characters needed to finish the piece.
  (define the-word-break (quad-attr-ref p world:word-break-key #f))
  (let ([p (apply piece (attr-delete (quad-attrs p) world:word-break-key) (group-quad-list p))]) ; so it doesn't propagate into subquads
    (if the-word-break
        (apply piece (quad-attrs p)
               (append (group-quad-list p) (let ([rendered-wb ((if before-break?
                                                             word-break->before-break
                                                             word-break->no-break) the-word-break)])
                                       (if (> (string-length (word-string rendered-wb)) 0) ; if rendered-wb is "", don't append it
                                           (list rendered-wb)
                                           empty))))
        p)))


;; shorthand
(define (render-piece-before-break p)
  (render-piece p #t))


;; helper macro to convert quad into word-break.
;; look up the break character and convert the quad based on what is found.
(define (render-word-break wb key)
  (let ([break-char (quad-attr-ref wb key)])
    (quad (if (whitespace? break-char) 'word-break 'word)
          (quad-attrs (quad-attr-remove* wb world:no-break-key world:before-break-key)) (list (quad-attr-ref wb key)))))

;; uses macro above in no-break mode.
(define (word-break->no-break wb)
  (render-word-break wb world:no-break-key))

;; uses macro above in before-break mode.
(define (word-break->before-break wb)
  (render-word-break wb world:before-break-key))

;; is this the last line? compare current line-idx to total lines
(define (last-line? line)
  (define line-idx (quad-attr-ref line world:line-index-key #f))
  (define lines (quad-attr-ref line world:total-lines-key #f))
  (and line-idx lines (= (add1 line-idx) lines)))

;; optical kerns are automatically inserted at the beginning and end of a line
;; (by the pieces->line function)
;; but may also be found elsewhere, imperatively (e.g., before an indent)
;; they allow certain characters to hang over the line margin.
;; optical kerns aren't considered when the line is being composed,
;; rather they are an adjustment added to a composed line.
;; the optical kern doesn't have left- or right-handed versions.
;; it just looks at quads on both sides and kerns them if appropriate.
;; in practice, only one will likely be used.
(define (render-optical-kerns exploded-line-quads)
  (define (overhang-width q)
    (if (and (word? q) (member (word-string q) world:hanging-chars))
        (* -1.0 (world:optical-overhang) (apply measure-text (word-string q) (font-attributes-with-defaults q)))
        0.0))
  (cond
    [(not (empty? exploded-line-quads))
     ;; after exploding, each quad will have a string with one character.
     (define shifted-lists (shifts exploded-line-quads '(1 -1)))
     (define lefts (first shifted-lists))
     (define rights (second shifted-lists))
     (for/list ([q-left (in-list lefts)][q (in-list exploded-line-quads)][q-right (in-list rights)])
       (if (optical-kern? q)
           (quad-attr-set q world:width-key (fl+ (overhang-width q-left) (overhang-width q-right)))
           q))]
    [else exploded-line-quads]))


;;; ultimately every line is filled to fit the whole measure.
;;; spacers are used to soak up extra space left over in a line.
;;; depending on where the spacers are inserted, different formatting effects are achieved.
;; e.g., left / right / centered / justified.
(define (insert-spacers-in-line line-in [alignment-override #f])
  ;; important principle: avoid peeking into quad-list to get attributes.
  ;; because non-attributed quads may be added.
  ;; here, we know that common attributes are hoisted into the line.
  ;; so rely on line attributes to get horiz alignment.
  (define key-to-use (if (and (last-line? line-in) (quad-has-attr? line-in world:horiz-alignment-last-line-key))
                         world:horiz-alignment-last-line-key
                         world:horiz-alignment-key))

  (define horiz-alignment (or alignment-override (quad-attr-ref line-in key-to-use (world:horiz-alignment-default))))
  (define default-spacer (spacer '()))
  (define-values (before middle after) (case horiz-alignment
                                         [(left) (values #f #f default-spacer)]
                                         [(right) (values default-spacer #f #f)]
                                         [(center) (values default-spacer #f default-spacer)]
                                         [(justified justify) (values #f default-spacer #f)]
                                         [else (values #f #f #f)]))
  (define (copy-with-attrs q attr-source)
    (define keys-to-ignore '(width)) ; width will be determined during fill routine
    (define filtered-attrs (and (quad-attrs attr-source)
                                (quad-attrs (apply quad-attr-remove* attr-source keys-to-ignore))))
    (quad (quad-name q) (merge-attrs (or filtered-attrs null) q) (quad-list q)))
  (apply line (quad-attrs line-in)
         (flatten-quadtree (let ([qs (group-quad-list line-in)])
                             (if (not (empty? qs))
                                 (list (if before (copy-with-attrs before (first qs)) null)
                                       (map (λ(q) (if (and middle (takes-justification-space? q))
                                                               (let ([interleaver (copy-with-attrs middle q)])
                                                                 (list interleaver q interleaver))
                                                               (list q))) qs)
                                       (if after (copy-with-attrs after (last qs)) null))
                                 qs)))))


;; installs the width in the quad.
;; this becomes the value reported by quad-width.
(define (embed-width q w)
  (quad-attr-set q world:width-key w))

;; installs the ascent in the quad.
(define (record-ascent q)
  (quad-attr-set q world:ascent-key (ascent q)))

;; helper function: doesn't need contract because it's already covered by the callers
(define (render-pieces ps)
  (define-values (initial-ps last-p) (split-last ps))
  ;;bg snoc
  (append (map render-piece initial-ps) (list (render-piece-before-break last-p))))


(define (calc-looseness total-width measure)
  (round-float (fl/ (fl- measure total-width) measure)))


;;; compose pieces into a finished line.
;;; take the contents of the rendered pieces and merge them.
;;; compute looseness for line as a whole.
;;; also add ascent to each component quad, which can be different depending on font & size.
(define (pieces->line ps measure-quad-proc)
  (define rendered-pieces (render-pieces ps))
  (cond
    [(not (empty? rendered-pieces))
     ;; handle optical kerns here to avoid resplitting and rejoining later.
     (define line-quads (append-map group-quad-list rendered-pieces))
     (define line-quads-maybe-with-opticals
       (if world:use-optical-kerns?
           (render-optical-kerns
            (let ([my-ok (list (optical-kern (quad-attrs (first line-quads))))]) ; take attrs from line, incl measure
              (append my-ok line-quads my-ok)))
           line-quads))
     (define merged-quads (join-quads line-quads-maybe-with-opticals))
     (define merged-quad-widths (map measure-quad-proc merged-quads)) ; 10% of function time

     ;(log-quad-debug "making pieces into line = ~v" (apply string-append (map quad->string merged-quads)))

     ;; if measure key isn't present, allow an error, because that's weird
     (when (not (quad-has-attr? (first merged-quads) world:measure-key))
       (error 'pieces->line "quad has no measure key: ~a" (first merged-quads)))

     (define measure (let ([val (quad-attr-ref (first merged-quads) world:measure-key)])
                       (if (flonum? val)
                           val
                           (error "got bad value for measure"))))
     (define looseness (calc-looseness (foldl fl+ 0.0 merged-quad-widths) measure))

     ;; quads->line function hoists common attributes into the line
     (let* ([new-line-quads (map embed-width merged-quads merged-quad-widths)]
            [new-line-quads (map record-ascent new-line-quads)]
            [new-line (quads->line new-line-quads)]
            [new-line (apply line (attr-change (quad-attrs new-line) (list world:line-looseness-key looseness)) (group-quad-list new-line))])
       new-line)]
    [else (line '())]))

;;; makes a wrap function by combining component functions.
(define (make-wrap-proc
               make-pieces-proc
               measure-quad-proc
               compose-line-proc
               find-breakpoints-proc)
  (λ(qs [measure #f])
    (if (not (empty? qs))
        (let* ([measure (or measure (quad-attr-ref/parameter (car qs) world:measure-key))]
               [qs (if (quad-has-attr? (car qs) world:measure-key)
                       qs
                       (map (λ(q) (quad-attr-set q world:measure-key measure)) qs))])
          ;(log-quad-debug "wrapping on measure = ~a" measure)
          (define pieces (make-pieces-proc qs))
          (define bps (find-breakpoints-proc (list->vector pieces) measure))
          (define broken-pieces (break-at pieces bps))
          (map (λ(broken-piece) (compose-line-proc broken-piece measure-quad-proc)) broken-pieces))
        (list (line '())))))

(define (make-piece-vectors pieces)
  (define pieces-measured
    (for/list ([p (in-vector pieces)])
      (define wb (quad-attr-ref p world:word-break-key #f))
      (vector
       ;; throw in 0.0 in case for/list returns empty
       (foldl fl+ 0.0 (for/list  ([q (in-list (group-quad-list p))])
                        (define str (quad->string q))
                        (if (equal? str "")
                             (quad-attr-ref q world:width-key 0.0)
                            (apply measure-text (quad->string q) (font-attributes-with-defaults q)))))
       (if wb (apply measure-text (quad-attr-ref wb world:no-break-key) (font-attributes-with-defaults wb)) 0.0)
       (if wb (apply measure-text (quad-attr-ref wb world:before-break-key) (font-attributes-with-defaults wb)) 0.0))))
  (values
   (for/vector ([p (in-list pieces-measured)])
     (fl+ (vector-ref p 0) (vector-ref p 1))) ; first = word length, second = nb length
   (for/vector ([p (in-list pieces-measured)])
     (fl+ (vector-ref p 0) (vector-ref p 2))))) ; first = word length, third = bb length

(define (make-trial-line pieces-rendered-widths pieces-rendered-before-break-widths i j)
  (let ([vec (vector-copy pieces-rendered-widths i j)])
    (vector-set! vec (sub1 (vector-length vec)) (vector-ref pieces-rendered-before-break-widths (sub1 j)))
    vec))

(define (get-line-width line)
  (round-float (foldl + 0.0 (vector->list line))))

;; top-level adaptive wrap proc.
;; first-fit and best-fit are variants.
(define (adaptive-fit-proc pieces measure [use-first? #t] [use-best? #t])
  ;; this is the winning performance strategy: extract the numbers first, then just wrap on those.
  ;; todo how to avoid re-measuring pieces later?
  ;; todo how to retain information about words per line and hyphen at end?
  (define-values (pieces-rendered-widths pieces-rendered-before-break-widths)
    (make-piece-vectors pieces))
  (define pieces-with-word-space (vector-map (λ(piece ) (and (quad-has-attr? piece world:word-break-key) (equal? (quad-attr-ref  (quad-attr-ref piece world:word-break-key) world:no-break-key) " "))) pieces))
  (define (make-first-fit-bps-and-widths)
    (define-values (reversed-bps reversed-widths)
      ;; breakpoints get stacked onto bps, so (car bps) is always the next starting point
      ;; thus use '(0) as a starting value to indicate that the first line starts at bp 0
      ;; bps will end up with at least two values (if all pieces fit on first line, bps = 0 and last bp)
      (for/fold ([bps '(0) ]
                 [line-widths  empty])
                ([j-1  (in-range (vector-length pieces))])
        (define line-starting-bp (car bps))
        (define line-width (get-line-width (make-trial-line pieces-rendered-widths
                                                            pieces-rendered-before-break-widths
                                                            line-starting-bp (add1 j-1))))
        (if (fl> line-width (fl* world:allowed-overfull-ratio measure))
            (values (cons j-1 bps) (cons line-width line-widths))
            (values bps line-widths))))
    (define bps (reverse reversed-bps))
    (values (if (not (empty? bps)) (cdr bps) empty) (reverse reversed-widths)))
  (define (fu-formula)
    (define line-count (length trial-line-widths))
    (cond
      [(<= line-count 2) 1.0] ; signals that first-fit is always OK with 1 or 2 lines
      [else ; only measure middle lines. we know bps has at least 2 bps
       (define looseness-stddev (fl (stddev (map (λ(x) (calc-looseness x measure)) (drop-right (drop trial-line-widths 1) 1)))))
       (define piece-count (vector-length pieces-rendered-widths))
       (define pieces-per-line (/ piece-count (sub1 line-count))) ; todo more accurate to count only pieces in middle
       (foldl fl+ 0.0 (list 2.2 (fllog (flabs looseness-stddev)) (fl* 0.09 (fl pieces-per-line))))])) ; the FU FORMULA

  ;; only buy first-fit-bps if use-first? is true.
  ;; use (values '(0) '(0.0)) as void-ish values that will typecheck properly.
  (define-values (first-fit-bps trial-line-widths) (if use-first? (make-first-fit-bps-and-widths) (values '(0) '(0.0))))

  (cond
    ;; possible outcomes at this branch:
    ;; adaptive wrap use-first and use-best are true, so first-fit-bps will exist, and fu-formula will be used.
    ;; first-fit wrap use-first is true but not use-best. So first-fit-bps will be returned regardless.
    ;; best-fit wrap use-first is false but use-best is true. So first-fit-bps will be skipped, and move on to best-fit.
    [(and use-first? (if use-best? (fl> (fu-formula) 0.0) #t))
     ;(log-quad-debug "first-fit breakpoints = ~a" first-fit-bps)
     first-fit-bps]
    [else
     (define ($penalty->value x)
       ($penalty-width x))
     (define initial-value ($penalty 0 0.0))
     ;(log-quad-debug "~a pieces to wrap = ~v" (vector-length pieces) (vector-map quad->string pieces))
     (define (penalty i j)
       (cond
         [(or (>= i j) ; implies negative or zero length line
              (> j (vector-length pieces))) ; exceeds available pieces
          ($penalty 0 (fl* -1.0 (fl i)))] ; ocm out of bounds signal
         [else
          (define penalty-up-to-i (ocm-min-entry ocm i)) ;;bg min-entry or min-value?
          (define last-piece-to-test (vector-ref pieces (sub1 j)))
          (define new-hyphen?
            (and (quad-has-attr? last-piece-to-test world:word-break-key)
                 (equal? (quad-attr-ref (quad-attr-ref last-piece-to-test world:word-break-key) world:before-break-key) "-")))
          (define cumulative-hyphens (if (not new-hyphen?)
                                         0
                                         (add1 ($penalty-hyphens penalty-up-to-i))))
          ($penalty
           cumulative-hyphens
           (round-float
            (apply + (list
                      (if (> cumulative-hyphens world:hyphen-limit)
                          (fl world:hyphen-penalty)
                          0.0)
                      (fl world:new-line-penalty)
                      ($penalty->value penalty-up-to-i)
                      (let ([line-width (get-line-width (make-trial-line pieces-rendered-widths pieces-rendered-before-break-widths i j))])
                        (cond
                          ;; overfull line huge penalty prevents break; multiplier is essential for monotonicity.
                          ;; multiply by -1 because line-width is longer than measure, thus diff is negative
                          [(fl> line-width (fl* world:allowed-overfull-ratio measure))
                           (fl* (fl- line-width measure) (flexpt 10.0 7.0))]
                          ;; standard penalty, optionally also applied to last line (by changing operator)
                          [((if world:last-line-can-be-short < <=) j (vector-length pieces))
                           (define words (fl (vector-count (λ(x) x) (vector-copy pieces-with-word-space i (sub1 j)))))
                           (fl/ (flexpt (fl- measure line-width) 2.0) (flmax 1.0 words))]
                          ;; only option left is (= j (vector-length pieces)), meaning we're on the last line.
                          ;; 0 penalty means any length is ok.
                          ;[(< (length pieces-to-test) (world:minimum-last-line-pieces)) 50000]
                          [else 0.0]))))))]))

     (define ocm  (make-ocm penalty $penalty->value initial-value))

     ;; starting from last position, ask ocm for position of row minimum (= new-pos)
     ;; collect this value, and use it as the input next time
     ;; until you reach first position.
     (define first-position 0)
     (define last-position (vector-length pieces))
     (define result (let loop  ([pos  last-position][acc  null])
                      (let ([next-pos (ocm-min-index ocm pos)]) ; first look ahead ...
                        (if (= next-pos first-position) ; therefore we're done
                            acc
                            (loop next-pos (cons next-pos acc))))))
     ;(log-quad-debug "best-fit breakpoints = ~a" result)
     result]))


(define wrap-first (make-wrap-proc
                            make-pieces
                            quad-width
                            pieces->line
                            (λ(x y ) (adaptive-fit-proc x y #t #f))))

;; wrap proc based on penalty function
(define wrap-best (make-wrap-proc
                           make-pieces
                           quad-width
                           pieces->line
                           (λ(x y ) (adaptive-fit-proc x y #f #t)))) ; note difference in boolean args

(define wrap-adaptive (make-wrap-proc
                               make-pieces
                               quad-width
                               pieces->line
                               adaptive-fit-proc))

;; build quad out to a given width by distributing excess into spacers
;; todo adjust this to work recursively, so that fill operation cascades down
;; and broaden type from just LineQuad
(define (fill starting-quad [target-width? #f])
  (define target-width (or target-width? (quad-attr-ref starting-quad world:measure-key)))
  (define subquads (group-quad-list starting-quad))
  (define-values (flexible-subquads fixed-subquads) (partition spacer? subquads)) ; only puts fill into spacers.
  (define width-used (foldl fl+ 0.0 (map quad-width fixed-subquads)))
  (define width-remaining (round-float (- target-width width-used)))
  (cond
    ;; check for zero condition because we want to divide by this number
    ;; if there's no spacers, put one in
    ;; todo go in two rounds, once for word spacers, and once for line spacers?
    ;; or separate the line alignment & word-spacing properties?
    [(fl= 0.0 (fl (length flexible-subquads))) (fill (insert-spacers-in-line starting-quad (world:horiz-alignment-default)) target-width)]
    [else (define width-per-flexible-quad (round-float (fl/ width-remaining (fl (length flexible-subquads)))))
          (define new-quad-list (map (λ(q) (if (spacer? q)
                                                                (quad-attr-set q world:width-key width-per-flexible-quad)
                                                                q)) subquads))
          (apply line (quad-attrs (quad-attr-set starting-quad world:width-key target-width)) new-quad-list)]))


;; add x positions to a list of fixed-width quads
;; todo adjust this to work recursively, so that positioning operation cascades down
(define (add-horiz-positions starting-quad)
  (define-values (new-quads final-width)
    (for/fold ([new-quads  empty][width-so-far  0.0])([q (in-list (group-quad-list starting-quad))])
      (values (cons (quad-attr-set q world:x-position-key width-so-far) new-quads) (round-float (fl+ (quad-width q) width-so-far)))))
  (quad (quad-name starting-quad) (quad-attrs starting-quad) (reverse new-quads)))
