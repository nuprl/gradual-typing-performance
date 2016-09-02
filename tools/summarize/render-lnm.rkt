#lang typed/racket/base

;; Tools for rendering L-N/M pictures

(provide
 render-bars
 render-means ;; hack
 render-exact
 render-exact*
 render-traces
 render-untyped-bars ;; hack
 render-typed/untyped
 render-dots
 render-lnm
 render-deliverable
 render-bars-xlabels

 data->pict
 ;; Build a picture from a list of pairs:
 ;;   1st component labels a dataset
 ;;   2nd component is a path-string to data
 ;; Optional argument tags the generated figure, because results are cached
 ;;  are re-used (when called with the same tag & dataset list)

 fname->title
 pict->png
)

;; -----------------------------------------------------------------------------

(require
 (for-syntax racket/base syntax/parse)
 (only-in racket/file file->value)
 (only-in racket/format ~r)
 (only-in racket/math exact-round pi)
 (only-in racket/port with-input-from-string open-output-nowhere)
 typed/racket/draw
 racket/flonum
 gtp-summarize/lnm-parameters
 gtp-summarize/lnm-plot
 gtp-summarize/path-util
 gtp-summarize/summary
 plot/typed/no-gui
 plot/typed/utils
 racket/cmdline
 racket/list
 racket/string
 typed/pict
 typed/racket/class
)

(define-type Pict pict)

(require/typed racket/serialize
  [serialize (-> Pict Any)]
  [deserialize (-> Any Pict)])

;; -----------------------------------------------------------------------------

(module dc-help racket
  (require racket/draw pict)
  (define (filled-triangle height #:color [pre-c '(0 0 0)])
    (define height/2 (/ height 2))
    (define c (apply make-object color% pre-c))
    (dc (λ (dc dx dy)
         (define old-brush (send dc get-brush))
         (define old-pen (send dc get-pen))
         (send dc set-brush
          (new brush% [style 'solid] [color c]))
         (send dc set-pen
          (new pen% [width 0] [color c]))
         (define path (new dc-path%))
         (send path move-to 0 0)
         (send path line-to height 0)
         (send path line-to height/2 height)
         (send path close)
         (send dc draw-path path dx dy)
         (send dc set-brush old-brush)
         (send dc set-pen old-pen)) height height))
  (provide filled-triangle))
(require/typed (submod "." dc-help)
  (filled-triangle (->* [Integer] [#:color (List Byte Byte Byte)] Pict)))

;; =============================================================================

(defparam *GRAPH-HSPACE* Positive-Integer 10)
(defparam *GRAPH-VSPACE* Positive-Integer 10)
(defparam *TITLE-VSPACE* Positive-Integer (assert (exact-round (/ (*GRAPH-VSPACE*) 2)) positive?))

(define DEBUG #t)
(define-syntax-rule (debug msg arg* ...)
  (when DEBUG (printf msg arg* ...) (newline)))

;; =============================================================================

;(: assert-false/L (-> (Parameterof Any) Void))
(define-syntax (assert-false/L stx)
  (syntax-parse stx
   [(_ ?PARAM:id)
    #:with param-str (syntax-e #'?PARAM)
    (syntax/loc stx
      (when (?PARAM)
        (raise-user-error 'render-lnm
          (format "Cannot make L-titles when parameter ~a is non-#f" 'param-str))))]))

;; Important parameters.
;; If these are changed, ignore cached pict
(: current-param-tag* (-> (Listof String)))
(define (current-param-tag*)
  (for/list : (Listof String)
      ([x (in-list (list (*PDF?*) (*SINGLE-PLOT?*)
                         (*AXIS-LABELS?*) (*L-LABELS?*) (*LINE-LABELS?*)
                         (*LEGEND?*) (*SHOW-PATHS?*) (*LOG-TRANSFORM?*)
                         (*HISTOGRAM?*) (*MAX-OVERHEAD*) (*NUM-SAMPLES*)
                         (*PLOT-WIDTH*) (*PLOT-HEIGHT*) (*Y-STYLE*)
                         (*N*) (*M*) (*L*)))])
    (format "~a" x)))

(: make-L-title* (-> (Listof String)))
(define (make-L-title*)
  (assert-false/L *SHOW-PATHS?*)
  (assert-false/L *SINGLE-PLOT?*)
  (assert-false/L *PDF?*)
  ;; --
  (define L (*L*))
  (cond
   [(not (list? L))
    (list (format "k = ~a (steps)" L))]
   [else
    ;; Normalize L data to list of Naturala
    (: normL (Listof Natural))
    (define normL
      (for/list : (Listof Natural)
                ([x : (U Natural (List Natural Plot-Pen-Style)) (in-list L)])
        (if (list? x) (car x) x)))
    (define num-L (length normL))
    (for/list : (Listof String)
              ([x (in-list normL)])
      (format "k = ~a" x))]))

;; old formatting function
(: l-index->string (-> Integer (-> Integer String)))
(define ((l-index->string num-L) i)
  (cond [(zero? i)
         (format "L = ~a" i)]
        [(= num-L i)
         (format "\t~a  (steps)" i)]
        [else
         (number->string i)]))

(: mytext (->* (String) ((U #f 'italic)) Pict))
(define (mytext str [mystyle #f])
  (text str
    (if mystyle
      ((inst cons 'italic String) mystyle (*TITLE-FONT-FACE*))
      (*TITLE-FONT-FACE*))
    (assert (+ 1 (*TABLE-FONT-SIZE*)) index?)))

;; Optional argument: list of Racket versions
(: make-legend (->* [] [(Listof String)] Pict))
(define (make-legend [version* '("# k-step D/U-usable")])
  ;; VSHIM separates 2 rows in the legend
  (define VSHIM (*TITLE-VSPACE*))
  (define HSHIM (* 3 (*GRAPH-HSPACE*)))
  (: myrule (-> String Any String Any  Pict))
  (define (myrule c-str c-val key val)
    (hc-append 0
      (colorize (mytext c-str) (format "~a" c-val))
      (mytext " rule: ")
      (mytext key 'italic)
      (mytext (format " = ~a" val))))
  (: myline (-> String Any String Integer Pict))
  (define (myline c-str c-val descr i)
    (hc-append 0
      (colorize
        (linewidth (integer->line-width i)
          (linestyle (integer->pen-style i)
            (hline HSHIM 5)))
        (cast c-val (List Byte Byte Byte)))
      (mytext (string-append " : " descr))))
  ;; LEGEND
  ;;  +--------------------------+
  ;;  | x  ---- yo   ---- lo     |
  ;;  | y                        |
  ;;  +--------------------------+
  (define N-RULE (myrule "orange" (*N-COLOR*) "N" (*N*)))
  (define M-RULE (myrule "grey" (*M-COLOR*) "M" (*M*)))
  (hc-append (* HSHIM 3.5)
    (vl-append VSHIM
      (mytext "x-axis: Overhead (vs. untyped)")
      (mytext "y-axis: % Acceptable configs."))
    (vl-append*/2 HSHIM VSHIM
      (for/list : (Listof Pict)
                ([v (in-list version*)]
                 [i (in-naturals 1)])
        (let-values (((color-txt color-val) (int->color i)))
          (myline color-txt color-val v i))))))

(: make-legend/points (-> (Listof (Vectorof String)) Pict))
(define (make-legend/points vec*)
  (define VSHIM (*TITLE-VSPACE*))
  (define HSHIM (* 3 (*GRAPH-HSPACE*)))
  (define version*
    (parse-version* (for*/list : (Listof String)
                               ([vec (in-list vec*)]
                                [str (in-vector vec)]
                                #:when (regexp-match? #rx"\\.rktd$" str))
                      str)))
  (for/fold : Pict
            ([acc : Pict (blank 0 0)])
            ([v (in-list version*)]
             [i (in-naturals 1)])
    (hc-append HSHIM
      acc
      (hc-append 0 (int->point-symbol i) (mytext (format " : ~a" v))))))

(: vl-append*/2 (-> Real Real (Listof Pict) Pict))
(define (vl-append*/2 h v p*)
  (cond
   [(null? p*)
    (blank h 0)]
   [(null? (cdr p*))
    (car p*)]
   [else
    (ht-append h
      (vl-append v (car p*) (cadr p*))
      (vl-append*/2 h v (cddr p*)))]))

(: vl-append* (-> Real (Listof Pict) Pict))
(define (vl-append* h p*)
  (if (null? p*)
    (blank h 0)
    (for/fold : Pict
              ([acc (car p*)])
              ([p (in-list (cdr p*))])
      (vl-append h acc p))))

(: ht-append* (-> Real (Listof Pict) Pict))
(define (ht-append* h p*)
  (if (null? p*)
    (blank h 0)
    (for/fold : Pict
              ([acc (car p*)])
              ([p (in-list (cdr p*))])
      (ht-append h acc p))))

(: hc-append* (-> Real (Listof Pict) Pict))
(define (hc-append* h p*)
  (if (null? p*)
    (blank h 0)
    (for/fold : Pict
              ([acc (car p*)])
              ([p (in-list (cdr p*))])
      (hc-append h acc p))))

(: int->point-symbol (-> Integer Pict))
(define (int->point-symbol i)
  (define size (cast (- (*POINT-SIZE*) 1) Positive-Index))
  (define c (cast (->pen-color i) (List Byte Byte Byte)))
  (case i
   [(1)
    ;; red triangledown
    (filled-triangle size #:color c)]
   [(2)
    ;; green circle
    (colorize (filled-ellipse size size #:draw-border? #f) c)]
   [else
    ;; blue square
    (colorize (filled-rectangle size size #:draw-border? #f) c)]))

(: int->style (-> Integer String))
(define (int->style i)
  (last (string-split (format "~a" (integer->pen-style i)) "-")))

;; Return a descriptive name and "actual" color value corresponding to an integer.
;;  (Should match the plot library's encoding from integers to colors)
(: int->color (-> Integer (Values String Any)))
(define (int->color i)
  (values
    (case i
     [(0) "black"]
     [(1) "red"]
     [(2) "green"]
     [(3) "blue"]
     [(4) "orange"]
     [(5) "navy"]
     [(6) "purple"]
     [(7) "magenta"]
     [else "???"])
    (->pen-color i)))

;; -----------------------------------------------------------------------------

;; Try to read a cached pict, fall back to making a new one.
(: data->pict (->* [(Listof (List String String))] [#:tag (U #f String)] Pict))
(define (data->pict data* #:tag [tag (*CACHE-TAG*)])
  (define title* (for/list : (Listof String) ([x (in-list data*)]) (car x)))
  (define rktd* (for/list : (Listof String) ([x (in-list data*)]) (cadr x)))
  (parameterize ([*CACHE-TAG* tag])
    (or
     (and tag (get-cached rktd*))
     (case #t  ;; TODO allow making deathscores etc.
      [(#t #f)
       (get-new-lnm-pict rktd* #:titles title*)]
      [(mean)
       (get-deathscore-pict rktd* #:titles title*)]
      [else
       (raise-user-error 'render-lnm "Unknown aggregation method")]))))

(: title-text (->* (String) (Real) Pict))
(define (title-text s [angle 0])
  (let ([face (*TABLE-FONT-FACE*)]
        [size (*TABLE-FONT-SIZE*)])
      (text s (cons 'bold face) size angle)))

(: subtitle-text (->* (String) (Real) Pict))
(define (subtitle-text s [angle 0])
  (let ([face (*TABLE-FONT-FACE*)]
        [size (assert (- (*TABLE-FONT-SIZE*) 1) index?)])
      (text s face size angle)))

(: sort-by-version (-> (Listof Summary) (Listof Summary)))
(define sort-by-version
  (let ([s<? (lambda ([s1 : Summary] [s2 : Summary])
               (string<? (summary->version s1) (summary->version s2)))])
    (lambda (S*)
      (sort S* s<?))))

;; Create a summary and L-N/M picts for a data file.
(: file->pict* (->* [(Listof String) #:title (U String #f)] (Listof Pict)))
(define (file->pict* data-file* #:title title)
  (define S* (for/list : (Listof Summary) ([d : String data-file*]) (from-rktd d)))
  (define L-pict* : (Listof Pict)
    (cond
     [(*SHOW-PATHS?*)
      (path-plot S*)]
     [(*DISCRETE?*)
      (list (plot-exact-configurations S*))]
     [else
      (lnm-plot S*)]))
  (if (*SINGLE-PLOT?*)
    L-pict*
    (let* ([VSHIM 2]
           [S (car S*)]
           [HSHIM (- (*GRAPH-HSPACE*) 2)]
           [first-lbl (title-text (format "~a" (or title (get-project-name S))))]
           [tu-ratio  (parameterize ([*TABLE-FONT-SIZE* (cast (- (*TABLE-FONT-SIZE*) 1) Positive-Index)])
                        (for/fold : Pict
                                  ([acc : Pict (blank 0 0)])
                                  ([S (in-list (sort-by-version S*))]
                                   [i (in-naturals 1)])
                          (define v (summary->version S))
                          (define t/u (~r (typed/untyped-ratio S) #:precision '(= 1)))
                          (define c
                            (let-values ([(str c0) (int->color i)])
                              (cast c0 (List Byte Byte Byte))))
                          (hc-append HSHIM
                                     acc
                                     (hc-append 0
                                       (title-text (format "v~a: " v))
                                       (colorize (title-text (format "~ax" t/u)) c)))))]
           [mid-lbl   (blank 0 (pict-height first-lbl))]
           [last-lbl  (title-text (format "~a configurations" (add-commas (get-num-configurations S))))])
      (cons
        (lt-superimpose
          (vr-append VSHIM tu-ratio (car L-pict*))
          first-lbl)
        (let loop : (Listof Pict)
                  ([p* (cdr L-pict*)])
          (cond
           [(null? p*)
            (printf "WARNING: have 1 L-pict, no place to put second label\n")
            '()]
           [(null? (cdr p*))
            ;; Last pict
            (list (vr-append VSHIM last-lbl (car p*)))]
           [else
            (cons (vl-append VSHIM mid-lbl (car p*)) (loop (cdr p*)))]))))))

(: format-filepath (-> (U #f String) String))
(define (format-filepath tag)
  ;; Check that cache-prefix exists
  (define p (*CACHE-PREFIX*))
  (define str* (string-split p "/"))
  (when (and (not (null? str*))
             (not (null? (cdr str*))))

    (for/fold : (U #f String)
              ([acc : (U #f String) #f])
              ([s (in-list (drop-right str* 1))])
      (if acc
        (let ([acc+ (string-append acc s)])
          (ensure-dir acc+)
          acc+)
        (begin
          (ensure-dir s)
          s))))
  (string-append p (or tag "") ".rktd"))

;; Save a pict, tagging with with `tag` and the `rktd*` filenames
(: cache-pict (-> Pict (Listof String) Void))
(define (cache-pict pict rktd*)
  (define tag (*CACHE-TAG*))
  (when tag
    (define filepath (format-filepath tag))
    (debug "Caching new pict at '~a'" filepath)
    (with-output-to-file filepath
      (lambda () (write (list rktd* (current-param-tag*) (serialize pict))))
      #:mode 'text
      #:exists 'replace)))

(: get-cached (-> (Listof String) (U Pict #f)))
(define (get-cached rktd*)
  (define filepath (format-filepath (*CACHE-TAG*)))
  (and (file-exists? filepath)
       (read-cache rktd* filepath)))

(define-type CachedPict (List (Listof String) (Listof String) Any))
(define-predicate cachedpict? CachedPict)

(: read-cache (-> (Listof String) String (U #f Pict)))
(define (read-cache rktd* filepath)
  (define tag+pict (file->value filepath))
  (cond
    [(cachedpict? tag+pict)
     (and (equal? rktd* (car tag+pict))
          (equal? (current-param-tag*) (cadr tag+pict))
          (debug "Reading cached pict from '~a'" filepath)
          (deserialize (caddr tag+pict)))]
    [else
     (error 'render-lnm (format "Malformed data in cache file '~a'" filepath))]))

(: zip-title* (-> (Listof String) (U #f (Listof String))
                   (Listof (Pairof (U #f String) (Listof String)))))
(define (zip-title* rktd* maybe-title*)
  (: title* (U (Listof String) (Listof #f)))
  (define title*
    (if maybe-title*
      (begin
        (unless (= (length maybe-title*) (length rktd*))
          (raise-user-error 'zip-title*
            (format "Have ~a datasets, but ~a titles: ~a" (length rktd*) (length maybe-title*) maybe-title*)))
        maybe-title*)
        (for/list : (Listof #f) ([x (in-list rktd*)]) #f)))
  ;; Combine duplicate titles
  (if (not (*GROUP-BY-TITLE?*))
    (list (cons (car title*) rktd*))
    (reverse
      (for/fold : (Listof (Pairof (U String #f) (Listof String)))
                ([acc : (Listof (Pairof (U String #f) (Listof String)))
                        '()])
                ([t (in-list title*)]
                 [r (in-list rktd*)])
        (cond
         [(and t (assoc t acc))
          (for/list ([t+r (in-list acc)])
            (define hd (car t+r))
            (if (and (string? hd) (string=? hd t))
              (list* t r (cdr t+r))
              t+r))]
         [else
          (cons (list t r) acc)])))))

(: get-deathscore-pict (->* [(Listof String)] [#:titles (U #f (Listof String))] Pict))
(define (get-deathscore-pict rktd* #:titles [maybe-title* #f])
  ;(define title+rktd* (zip-title* rktd* maybe-title*))
  (raise-user-error 'render-lnm "cannot make death scores right now"))
;  (define S*
;    (for/list : (Listof Summary)
;              ([d : String rktd*])
;      (from-rktd d)))
;  (car (make-plot death-plot S*)))

;; Create a pict, cache it for later use
(: get-new-lnm-pict (->* [(Listof String)] [#:titles (U #f (Listof String))] Pict))
(define (get-new-lnm-pict rktd* #:titles [maybe-title* #f])
  (define title+rktd* (zip-title* rktd* maybe-title*))
  ;; Get all picts. (Each call to lnm-plot returns a list)
  (define pict**
    (for/list : (Listof (Listof Pict))
              ([title+rktd : (Pairof (Option String) (Listof String)) (in-list title+rktd*)])
      (collect-garbage 'major)
      (printf "INFO: building pict for data files '~a'\n" (cdr title+rktd))
      (file->pict* (cdr title+rktd) #:title (car title+rktd))))
  ;; Align all picts vertically first
  (define column* : (Listof Pict)
    (or (for/fold : (U #f (Listof Pict))
              ([prev* : (U #f (Listof Pict)) #f])
              ([pict* : (Listof Pict) (in-list pict**)])
      (if prev*
          ;; Right-align the old picts with the new ones
          (for/list : (Listof Pict)
                    ([old (in-list prev*)]
                     [new (in-list pict*)])
            (vr-append (*GRAPH-VSPACE*) old new))
          ;; Generate titles.
          (if (*L-LABELS?*)
            (for/list : (Listof Pict)
                      ([l-str (in-list (make-L-title*))]
                       [new (in-list pict*)])
              (vc-append (*TITLE-VSPACE*)
                         (text l-str
                               (cons 'bold (*TITLE-FONT-FACE*))
                               (*TITLE-FONT-SIZE*))
                         new))
            pict*)))
         (error 'invariant)))
  ;; Paste the columns together
  (define pict0 : Pict
    (or
      (for/fold : (U #f Pict)
                ([prev-pict : (U #f Pict) #f])
                ([c column*])
        (if prev-pict
            (hc-append (*GRAPH-HSPACE*) prev-pict c)
            c))
      (error 'invariant)))
  (define pict/legend
    (if (*LEGEND?*)
      (vc-append (*GRAPH-VSPACE*)
        pict0
        (make-legend (parse-version* rktd*)))
      pict0))
  (cache-pict pict/legend rktd*)
  pict/legend)

(: parse-version* (-> (Listof String) (Listof String)))
(define (parse-version* rktd*)
  (define v*
    (for/fold ([acc : (Listof String) '()])
              ([rktd (in-list rktd*)])
      (define v (string->version rktd))
      (if v
        (let ([v+ (string-append "v" v)])
          (if (not (member v+ acc))
            (cons v+ acc)
            acc))
        acc)))
  (sort v* string<?))

;; =============================================================================
(define-syntax-rule (reads l)
  (with-input-from-string (assert l string?) read))

(: filter-valid-filenames (-> (Listof Any) (Listof String)))
(define (filter-valid-filenames arg*)
  (for/list : (Listof String)
            ([fname (in-list arg*)]
             #:when (and (string? fname)
                         (valid-filename? fname)))
    fname))

(: valid-filename? (-> String Boolean))
(define (valid-filename? fname)
  (cond
   [(= 0 (string-length fname))
    #f]
   [(and (file-exists? fname)
         (regexp-match? #rx"\\.rktd$" fname))
    #t]
   [else
    (printf "Skipping invalid/missing file '~a'\n" fname)
    #f]))

;; Remove everything after the first . or - in the filename
(: fname->title (-> Path-String String))
(define (fname->title fname)
  ;; Pretty inefficient
  (car (string-split (path->project-name fname) ".")))

;; -----------------------------------------------------------------------------
(define-type Real** (Listof (Listof Real)))

;;; For command-line clients
;(: renderbar (-> (Listof Path-String) Symbol Pict))
;(define (renderbar dir* bar-type)
;  (assert-directory-exists* dir*)
;  (define-values (S->data render-proc)
;    (case bar-type
;     [(ratio)
;      ]
;     [(mean)
;      ]
;     [(max)
;      ]
;     [(untyped)
;      ]
;     [else
;      (raise-user-error 'renderbar "Unknown barchart type '~a'" bar-type)]))
;  (define rktd**
;    (for/list : (Listof (Listof String))
;              ([d (in-list dir*)])
;      (define str (if (string? d) d (path->string d)))
;      (glob (string-append str "/*.rktd"))))
;  (define name*
;    (for/list : (Listof String)
;              ([rktd (in-list (car rktd**))])
;      (fname->title rktd)))
;  (define data**
;    (for*/list : Real**
;               ([rktd* 
;  (render-proc name* data**))

(: render-untyped-bars (-> (Listof String) Real** Pict))
(define (render-untyped-bars name* r**)
  (parameterize ([*GRAPH-VSPACE* (assert (/ (*GRAPH-VSPACE*) 2) index?)])
    (vr-append (* (*GRAPH-VSPACE*) 6)
      (parameterize ([*Y-STYLE* 'X])
        (render-runtime-bars r** #:title "(Normalized) Untyped"))
      (parameterize ([*Y-STYLE* '%])
        (render-runtime-bars r** #:title "(% Diff) Untyped"))
      (render-bars-legend (* 3.3 (*GRAPH-HSPACE*)) name*))))

(: render-bars (-> (Listof String) Real** Real** Real** Pict))
(define (render-bars name* ratio** mean** max**)
  (parameterize ([*GRAPH-VSPACE* (assert (/ (*GRAPH-VSPACE*) 2) index?)])
    (vr-append (* (*GRAPH-VSPACE*) 6)
      (render-typed/untyped-ratio-bars ratio**)
      (render-mean-overhead-bars mean**)
      (render-max-overhead-bars max**)
      (render-bars-legend (* 3 (*GRAPH-HSPACE*)) name*))))

(: render-typed/untyped-ratio-bars (-> Real** Pict))
(define (render-typed/untyped-ratio-bars ratio**)
  (define VTHIN (*GRAPH-VSPACE*))
  (vl-append VTHIN
    (hb-append VTHIN
      (title-text "Typed/Untyped Ratio")
      (subtitle-text "quotient of fully-typed and fully-untyped performance"))
    (hc-append 0
      (blank SHIM-FOR-BARCHART-ALIGNMENT 0)
      (lnm-bar ratio** 'ratio))))

(: render-mean-overhead-bars (-> Real** Pict))
(define (render-mean-overhead-bars r**)
  (render-overhead-bars r** #:title "Average"
    #:subtitle "computed over all gradually typed configurations"))

(: render-max-overhead-bars (-> Real** Pict))
(define (render-max-overhead-bars r**)
  (render-overhead-bars r** #:title "Max"
    #:subtitle "worst-case of any gradually typed configuration"))

(: render-runtime-bars (->* [Real**] [#:title String #:subtitle (U String #f)] Pict))
(define (render-runtime-bars data** #:title [title "???"] #:subtitle [subtitle #f])
  (render-generic-bars data** 'runtime title subtitle))

(: render-overhead-bars (->* [Real**] [#:title String #:subtitle (U String #f)] Pict))
(define (render-overhead-bars data** #:title [title "???"] #:subtitle [subtitle #f])
  (render-generic-bars data** 'overhead title subtitle))

(: render-generic-bars (-> Real** BarType String (U #f String) Pict))
(define (render-generic-bars data** bar-type title subtitle)
  (define VTHIN (*GRAPH-VSPACE*))
  (vl-append VTHIN
    (hb-append VTHIN
      (title-text (string-append title " " (string-titlecase (symbol->string bar-type))))
      (if subtitle (subtitle-text subtitle) (blank 0 0)))
    (lnm-bar data** bar-type)))

(: render-bars-legend (-> Real (Listof String) Pict))
(define (render-bars-legend hspace name*)
  (ht-append hspace
    (render-bars-color-key)
    (render-bars-xlabels hspace name*)))

(: render-bars-color-key (->* [] [#:vertical? Boolean] Pict))
(define (render-bars-color-key #:vertical? [vertical? #t])
  (define HSHIM (* 3 (*GRAPH-HSPACE*)))
  (define pict*
    (for/list : (Listof Pict)
              ([i (in-range 1 4)])
      (hb-append 0
        (integer->bar-swatch i)
        (mytext (format " : ~a" (integer->rkt-version i))))))
  (if vertical?
    (vl-append* (*TITLE-VSPACE*) pict*)
    (hc-append* HSHIM pict*)))

(: integer->bar-swatch (-> Integer Pict))
(define (integer->bar-swatch i)
  (define sty (integer->brush-style i))
  (define col* (cast (->pen-color i) (List Byte Byte Byte)))
  (define col (make-object color% (car col*) (cadr col*) (caddr col*)))
  (define w 4)
  (define h 14)
  (dc (lambda (dc dx dy)
       (define old-brush (send dc get-brush))
       (define old-pen (send dc get-pen))
       (send dc set-brush
        (new brush% [style sty]
                    [color col]))
       (send dc set-pen
        (new pen% [width 1] [color col]))
       (define path (new dc-path%))
       (send path move-to 0 0)
       (send path line-to w 0)
       (send path line-to w h)
       (send path line-to 0 h)
       (send path close)
       (send dc draw-path path dx dy)
       (send dc set-brush old-brush)
       (send dc set-pen old-pen))
      w h))

(: integer->rkt-version (-> Integer String))
(define (integer->rkt-version i)
  (case i
   [(1) "v6.2"]
   [(2) "v6.3"]
   [(3) "v6.4"]
   [else (raise-user-error "Unrecognized version ~a" i)]))

(: render-bars-xlabels (-> Real (Listof String) Pict))
(define (render-bars-xlabels hspace name*)
  (define VSHIM (exact-round (/ (*GRAPH-VSPACE*) 2)))
  (define pict*
    (for/list : (Listof Pict)
              ([name (in-list name*)]
               [i (in-naturals 1)])
      (define name+ (if (regexp-match? #rx"^zordoz" name) "zordoz" name))
      (text (format "~a. ~a" (integer->letter i) name+) (*TABLE-FONT-FACE*) (*TABLE-FONT-SIZE*))))
  (ht-append* hspace
    (for/list : (Listof Pict)
              ([p* (in-list (split-list 4 pict*))])
      (vl-append* VSHIM p*))))

;; -----------------------------------------------------------------------------

(: render-dots (-> (Listof (Listof Path-String)) Pict))
(define (render-dots rktd**)
  (vl-append* (*GRAPH-VSPACE*)
    (for/list ([rktd* (in-list rktd**)])
      (collect-garbage 'major)
      (render-dot rktd*))))

(: render-dot (-> (Listof Path-String) Pict))
(define (render-dot rktd*)
  (define title (title-text (fname->title (car rktd*))))
  (define S*
    (for/list : (Listof Summary)
              ([ps (in-list rktd*)])
      (from-rktd ps)))
  (define ratio* (map typed/untyped-ratio S*))
  (define mean* (map avg-overhead S*))
  (define max* (map max-overhead S*))
  (define num** (list ratio* mean* max*))
  (define HSHIM (*GRAPH-HSPACE*))
  (hc-append (* 3 HSHIM)
    title
    (hc-append* (* 6 HSHIM)
      (for/list ([num* (in-list num**)]
                 [i (in-naturals 1)])
        (define n0 (car num*))
        (define (diameter (area : Real)) : Flonum
          (fl* 2.0 (flsqrt (fl/ (real->double-flonum area) 3.14))))
        (define target-area 100)
        (define-values (_str color) (int->color i))
        (colorize
          (hc-append* HSHIM
            (for/list ([n (in-list num*)])
              (disk (diameter (* target-area (+ 1 (/ (- n n0) n0))))
               #:draw-border? #f)))
          (cast color (List Byte Byte Byte)))))))

;; -----------------------------------------------------------------------------

(define-syntax-rule (simple-commandline-plot make-plot)
  (lambda ([vec : (Vectorof String)])
    (ensure-dir "./compiled")
    (command-line
     #:program "render-plot"
     #:argv vec
     #:args FNAME*
     ;; -- Filter valid arguments, assert that we got anything to render
     (define arg* (filter-valid-filenames FNAME*))
     (when (null? arg*)
       (raise-user-error "Usage: render-plot.rkt DATA.rktd ..."))
     ;; -- Create a pict
     (vl-append (*TITLE-VSPACE*)
       (title-text (fname->title (vector-ref vec 0)))
       (make-plot (for/list : (Listof Summary) ([r (in-list arg*)]) (from-rktd r)))))))

(: render-exact (-> (Vectorof String) Pict))
(define render-exact (simple-commandline-plot plot-exact-configurations))

;; Make a separate plot for each argument, return the appended picts
(: render-exact* (-> (Listof (Vectorof String)) Pict))
(define (render-exact* vec*)
  (define pict*
    ;; reverse-map
    (for/fold : (Listof Pict)
              ([acc : (Listof Pict) '()])
              ([vec (in-list vec*)])
      (cons (render-exact vec) acc)))
  (define legend (make-legend/points vec*))
  (for/fold : Pict
            ([acc legend])
            ([p (in-list pict*)])
    (vc-append (*GRAPH-VSPACE*) p acc)))

;; TODO command-line friendly API
(: render-typed/untyped (-> (Listof (Vectorof String)) Pict))
(define (render-typed/untyped vec*)
  (define pict (plot-typed/untyped-ratio vec*))
  (define legend (make-legend/points vec*))
  (vc-append (*GRAPH-VSPACE*) pict legend))

(: render-deliverable (-> Nonnegative-Real (Listof (Vectorof String)) Pict))
(define (render-deliverable D vec*)
  (define bm-name* (for/list : (Listof String)
                             ([vec (in-list vec*)])
                     (path->project-name (vector-ref vec 0))))
  (define pict (plot-deliverable D vec*))
  (define legend (render-bars-color-key #:vertical? #f))
  (vc-append (*GRAPH-VSPACE*) pict legend))

(: render-traces (-> (Vectorof String) Pict))
(define render-traces (simple-commandline-plot plot-traces))

(: render-means (-> (Vectorof String) Pict))
(define render-means (simple-commandline-plot plot-mean-bars))

;; -----------------------------------------------------------------------------

(: pict->png (-> Pict Path-String Boolean))
(define (pict->png p path)
  (send (pict->bitmap p) save-file path 'png))

(: render-lnm (-> (Vectorof String) Pict))
(define (render-lnm vec)
  (ensure-dir "./compiled")
  (command-line
   #:program "render-lnm"
   #:argv vec
   #:once-each
   [("-o" "--output") o-param
    "Location to save results"
    (*OUTPUT* (cast o-param String))]
   ;; TODO (here I am, testing)
   [("-p" "--path" "--paths")
    "Count paths instead of configurations"
    (*SHOW-PATHS?* #t)]
   ;; TODO enable aggregation
   ;[("-d" "--deathscore")
   ; sym
   ; "Create a deathscore, valid params: 'mean"
   ; (*TODO* sym)]
   [("--legend")
    legend
    "#t/#f = show/hide legend"
    (*LEGEND?* (assert (reads legend) boolean?))]
   [("--discrete")
    "Plot discrete points, instead of line"
    (*DISCRETE?* #t)]
   [("--error" "--error-bars")
    "Enable error bars"
    (*ERROR-BAR?* #t)]
   [("--split")
    "Use different plot for each L"
    (*SINGLE-PLOT?* #f)]
   [("--single")
    "Put all L on the same plot"
    (*SINGLE-PLOT?* #t)]
   [("--hist" "--histogram" "-H")
    "Show CDF as a histogram"
    (*HISTOGRAM?* #t)]
   [("--log")
    "Plot x-axis on a log scale"
    (*LOG-TRANSFORM?* #t)]
   [("--labels")
    lbl
    "Enable / Disable all labels"
    (let ([b (assert (reads lbl) boolean?)])
     (*AXIS-LABELS?* b)
     (*LINE-LABELS?* b)
     (*L-LABELS?*    b))]
   [("--axis-labels")
    lbl
    "#t/#f = show/hide axis labels"
    (*AXIS-LABELS?* (assert (reads lbl) boolean?))]
   [("--line-labels")
    lbl
    "#t/#f = show/hide line labels"
    (*LINE-LABELS?* (assert (reads lbl) boolean?))]
   [("--l-label")
    "Show L labels above each plot"
    (*L-LABELS?* #t)]
   [("-g" "--group-by-title")
    gb
    "If #f, put everything on the same plot"
    (*GROUP-BY-TITLE?* (assert (reads gb) boolean?))]
   [("--cutoff")
    c
    "Set red line with a number in [0,1] (#f by default)"
    (*CUTOFF-PROPORTION* (assert (reads c) real?))]
   [("--pdf")
    "Plot derivative of L-N/M plot (instead of cumulative)"
    (*PDF?* #t)]
   [("-y" "--y-style")
    y
    "Display 'count or '% on y-axis"
    (*Y-STYLE* (cast (string->symbol (assert y string?)) Y-Style))]
   [("-N")
    n
    "Set line for N (#f by default)"
    (*N* (assert (reads n) real?))]
   [("-M")
    m
    "Set line for M (#f by default)"
    (*M* (assert (reads m) real?))]
   [("-L")
    l
    "Set L values, may be natural, (listof natural) or (listof (list natural pen-style))"
    (let ([val (reads l)])
      (cond
       [(exact-nonnegative-integer? val)
        (*L* val)]
       [(and (list? val) (andmap exact-nonnegative-integer? val))
        (*L* (cast val (Listof Index)))]
       [else
        (*L* (cast val (Listof (List Index Plot-Pen-Style))))]))]
   [("--max" "--max-overhead")
    mx
    "Largest x-axis value"
    (*MAX-OVERHEAD* (assert (assert (reads mx) index?) positive?))]
   [("-c" "--cache")
    c
    "Cache generated pict for future calls"
    (*CACHE-TAG* (cast c (U #f String)))]
   [("--samples" "--num-samples")
    sm
    "Number of samples along x-axis"
    (*NUM-SAMPLES* (assert (assert (reads sm) index?) positive?))]
   #:args FNAME*
   ;; -- Filter valid arguments, assert that we got anything to render
   (define arg* (filter-valid-filenames FNAME*))
   (when (null? arg*)
     (raise-user-error "Usage: render-lnm.rkt DATA.rktd ..."))
   ;; -- Create a pict
   (define P
      (data->pict
        (for/list : (Listof (List String String))
                  ([fname (in-list arg*)])
          (list (fname->title fname) fname))))
   P))

(module+ main
  (let ([p (render-lnm (current-command-line-arguments))])
    (pict->png p (*OUTPUT*)))
)

;; =============================================================================

(module+ test
  (require typed/rackunit)

  ;; -- format-filepath
  (let ([prefix "compiled/test"])
    (parameterize ([*CACHE-PREFIX* prefix])
      (check-equal? (format-filepath #f) (string-append prefix ".rktd"))
      (check-equal? (format-filepath "hello") (string-append prefix "hello.rktd"))))

  ;; -- zip-title*
  (define-syntax-rule (check-zip-title* [a ... c] ...)
    (begin (check-equal? (zip-title* a ...) c) ...))
  (check-zip-title*
   ['() #f
    '()]
   ;; -- no titles
   ['("a" "b" "c" "d") #f
    '((#f "a") (#f "b") (#f "c") (#f "d"))]
   ;; -- unique titles
   ['("a" "b" "c" "d") '("1" "2" "3" "4")
    '(("1" "a") ("2" "b") ("3" "c") ("4" "d"))]
   ['("a-1" "b-1") '("hi" "hi")
   '(("hi" "b-1" "a-1"))]
   ['("a.0-1" "b.1-1") '("hi" "hi")
   '(("hi" "b.1-1" "a.0-1"))])

  (check-exn exn:fail:user?
    (lambda () (zip-title* '("A") '())))

  ;; -- filter-valid-filenames
  (parameterize ([current-output-port (open-output-nowhere)])
    (check-equal? (filter-valid-filenames '(a b c)) '())
    (check-equal? (filter-valid-filenames '("foo.rktd" "bar.rkt")) '())
    (check-equal? (filter-valid-filenames '("test/echo-data.rktd")) '("test/echo-data.rktd"))
  )

  ;; -- valid-filename?
  (parameterize ([current-output-port (open-output-nowhere)])
    (check-true (valid-filename? "test/echo-data.rktd"))

    (check-false (valid-filename? ""))
    (check-false (valid-filename? "no.r"))
  )

)
