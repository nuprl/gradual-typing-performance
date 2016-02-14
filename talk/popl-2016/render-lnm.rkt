#lang typed/racket/base

;; Specific tools for rendering L-N/M pictures in the current paper.

;; We currently use this two ways:
;; - The paper (typed-racket.scrbl) calls 'data->pict' to create an image
;; - From the command-line, call `render-lnm.rkt -o FIG.png DATA.rktd ...`
;;   to create a figure named `FIG.png` from the data files `DATA.rktd ...`
;; -- Use the -p option to count performant paths rather than lattice points
;; -- If two .rktd files with the same module graph are given, plots those on
;;     on the same graph

(provide
 render-lnm

 data->pict
 ;; Build a picture from a list of pairs:
 ;;   1st component labels a dataset
 ;;   2nd component is a path-string to data
 ;; Optional argument tags the generated figure, because results are cached
 ;;  are re-used (when called with the same tag & dataset list)

 ;; -- Global parameters for the generated figures.
 PARAM-N
 PARAM-M
 PARAM-L
 PARAM-MAX-OVERHEAD
 PARAM-NUM-SAMPLES
)

;; -----------------------------------------------------------------------------

(require
 "scripts/lnm-plot.rkt"
 (only-in "scripts/summary.rkt" from-rktd summary->pict path->project-name Summary)
 (only-in racket/file file->value)
 (only-in racket/string string-join)
 (only-in racket/port with-input-from-string)
 "scripts/pict-types.rkt"
 (only-in plot/no-gui Plot-Pen-Style)
)

(require/typed racket/serialize
  [serialize (-> Pict Any)]
  [deserialize (-> Any Pict)])

;; =============================================================================
;; --- constants

;; Experiment parameters
;; TODO parameters for max, num-samples
(define PARAM-MAX-OVERHEAD 20)
(define PARAM-NUM-SAMPLES 60)

(define PARAM-PDF? : (Parameterof Boolean) (make-parameter #f))
(define PARAM-SPLIT? : (Parameterof Boolean) (make-parameter #f))
(define PARAM-STATS? : (Parameterof Boolean) (make-parameter #t))
(define PARAM-AXIS-LABELS? : (Parameterof Boolean) (make-parameter #t))
(define PARAM-L-LABELS? : (Parameterof Boolean) (make-parameter #f))
(define PARAM-LEGEND? : (Parameterof Boolean) (make-parameter #t))
(define PARAM-CUTOFF : (Parameterof (U #f Real)) (make-parameter 0.6))
(define PARAM-N : (Parameterof (U #f Natural)) (make-parameter 3))
(define PARAM-M : (Parameterof (U #f Natural)) (make-parameter 10))
(define PARAM-L : (Parameterof (U Natural (Listof Natural) (Listof (List Natural Plot-Pen-Style)))) (make-parameter 0))
(define extra-summary : (Parameterof (U #f String)) (make-parameter #f))

;(define H 100)
;(define W 130)
(define H 200)
(define W 250)

(: *show-paths?* (Parameterof Boolean))
(define *show-paths?* (make-parameter #f))

(: *aggregate* (Parameterof (U Symbol #f)))
(define *aggregate* (make-parameter #f))

(define DEBUG #t)
(define-syntax-rule (debug msg arg* ...)
  (when DEBUG (printf msg arg* ...) (newline)))

;; =============================================================================

(define-syntax-rule (make-plot plot-proc S)
  (plot-proc S
	     #:summary (and (extra-summary)
			    (from-rktd (assert (extra-summary) string?)))
             #:L (PARAM-L)
             #:N (PARAM-N)
             #:M (PARAM-M)
             #:max-overhead PARAM-MAX-OVERHEAD
             #:num-samples PARAM-NUM-SAMPLES
             #:font-face FONT-FACE
             #:font-size GRAPH-FONT-SIZE
             #:split-plot? (PARAM-SPLIT?)
             #:pdf? (PARAM-PDF?)
             #:labels? (PARAM-AXIS-LABELS?)
             #:cutoff-proportion (PARAM-CUTOFF)
             #:plot-height H
             #:plot-width W))

(define FONT-FACE "Roboto, Light")

(define GRAPH-FONT-SIZE 6)
(define TEXT-FONT-SIZE 10)
(define GRAPH-HSPACE 10)
(define GRAPH-VSPACE 20)
(define TITLE-STYLE FONT-FACE)
(define TITLE-SIZE (+ 2 TEXT-FONT-SIZE))
(define TITLE-VSPACE (/ GRAPH-VSPACE 2))

(define CACHE-PREFIX "./compiled/lnm-cache-")

;; =============================================================================

(define (make-L-title*)
  (error "cannot make L titles right now"))
  ;(define x (PARAM-L))
  ;(cond
  ; [(list? 
  ;(define num-l (length (PARAM-L)))
  ;(: l-index->string (-> Integer String))
  ;(define (l-index->string i)
  ;  (cond [(zero? i)
  ;         (format "L = ~a" i)]
  ;        [(= num-l i)
  ;         (format "\t~a  (steps)" i)]
  ;        [else
  ;         (number->string i)]))
  ;(for/list : (Listof String)
  ;          ([x (in-list (PARAM-L))])
  ;  (l-index->string x)))

(define (make-legend)
  (define VSHIM (/ TITLE-VSPACE 3))
  (: mytext (->* (String) (Any) Pict))
  (define (mytext str [mystyle #f])
      (text str
        (if mystyle (cons mystyle TITLE-STYLE) TITLE-STYLE)
        (+ 1 TEXT-FONT-SIZE)))
  ;; TODO spacing is sometimes wrong... recompiling fixes
  (hc-append (* 6 GRAPH-HSPACE)
    (vl-append VSHIM
     (mytext "x-axis: overhead")
     (mytext "y-axis: # configs"))
    (vl-append VSHIM
     (hc-append 0
       (colorize (mytext "red") "orangered")
       (mytext (format " line: ~a% of configs." (round (* 100 (or (PARAM-CUTOFF) (error 'cutoff)))))))
     (hc-append 0
       (colorize (mytext "blue") "navy")
       (mytext " line: # ")
       (mytext "L" 'italic)
       (mytext "-step ")
       (mytext "N" 'italic)
       (mytext "/")
       (mytext "M" 'italic)
       (mytext "-usable")))
    (vl-append VSHIM
     (hc-append 0
       (colorize (mytext "green") "forestgreen")
       (mytext " line: ")
       (mytext "N" 'italic)
       (mytext (format "=~a" (PARAM-N))))
     (hc-append 0
       (colorize (mytext "yellow") "goldenrod")
       (mytext " line: ")
       (mytext "M" 'italic)
       (mytext (format "=~a" (PARAM-M)))))))

;; -----------------------------------------------------------------------------

;; Try to read a cached pict, fall back to making a new one.
(: data->pict (->* [(Listof (List String String))] [#:tag String] Pict))
(define (data->pict data* #:tag [tag ""])
  (define title* (for/list : (Listof String) ([x (in-list data*)]) (car x)))
  (define rktd* (for/list : (Listof String) ([x (in-list data*)]) (cadr x)))
  ;; TODO use options in tag -- new options should invalidate the cache
  (or (get-cached rktd* #:tag tag)
      (if (*aggregate*)
        (error "deathscore not working right now")
        ;(get-deathscore-pict rktd* #:tag tag #:titles title*)
        (get-new-lnm-pict rktd* #:tag tag #:titles title*))))

;; Create a summary and L-N/M picts for a data file.
(: file->pict* (->* [(Listof String) #:title (U String #f)] (Listof Pict)))
(define (file->pict* data-file* #:title title)
  (define S* (for/list : (Listof Summary) ([d : String data-file*]) (from-rktd d)))
  (define S-pict : Pict
    (if (and (not (null? S*)) (null? (cdr S*)) (PARAM-STATS?))
      (let ([p (summary->pict (car S*)
                     #:title title
                     #:font-face FONT-FACE
                     #:font-size TEXT-FONT-SIZE
                     #:N (or (PARAM-N) (error "need N"))
                     #:M (or (PARAM-M) (error "need M"))
                     #:width (* 0.6 W)
                     #:height H)])
        (vc-append 0 p (blank 0 (- H (pict-height p)))))
      (blank 0 0)))
  (define L-pict*
    ;; TODO there's only a 3-character difference between the branches...
    ;;  I tried making plot-fn = (if show-paths? path-plot lnm-plot),
    ;;  but a U-type can't be applied!
    (if (*show-paths?*)
      (error "no pth plot");(make-plot path-plot S*)
      (make-plot lnm-plot (car S*)))) ;; TODO 
  (cons S-pict L-pict*))

(: format-filepath (-> (U #f String) String))
(define (format-filepath tag)
  (string-append CACHE-PREFIX (or tag "") ".rktd"))

;; Save a pict, tagging with with `tag` and the `rktd*` filenames
(: cache-pict (-> Pict (Listof String) (U #f String) Void))
(define (cache-pict pict rktd* tag)
  (define filepath (format-filepath tag))
  (debug "Caching new pict at '~a'" filepath)
  (with-output-to-file filepath
    (lambda () (write (list rktd* (current-param-tag*) (serialize pict))))
    #:mode 'text
    #:exists 'replace))

(: get-cached (->* [(Listof String)] [#:tag String] (U Pict #f)))
(define (get-cached rktd* #:tag [tag ""])
  (define filepath (format-filepath tag))
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

(: current-param-tag* (-> (Listof String)))
(define (current-param-tag*)
  (for/list : (Listof String)
      ([x (in-list (list (PARAM-PDF?) (PARAM-SPLIT?) (PARAM-STATS?)
                         (PARAM-AXIS-LABELS?) (PARAM-L-LABELS?)
                         (PARAM-LEGEND?) (PARAM-CUTOFF) (PARAM-N) (PARAM-M) (PARAM-L)))])
    (format "~a" x)))

(: zip-title* (->* [(Listof String) (U #f (Listof String))]
                       [#:collapse? Boolean]
                       (Listof (Pairof (U #f String) (Listof String)))))
(define (zip-title* rktd* maybe-title* #:collapse? [collapse? #f])
  (: title* (U (Listof String) (Listof #f)))
  (define title*
    (if maybe-title*
      (begin
        (unless (= (length maybe-title*) (length rktd*))
          (error 'group-by-title (format "Have ~a datasets, but ~a titles: ~a" (length rktd*) (length maybe-title*) maybe-title*)))
        maybe-title*)
        (for/list : (Listof #f) ([x (in-list rktd*)]) #f)))
  ;; Combine duplicate titles
  (if collapse?
    (for/fold : (Listof (Pairof (U String #f) (Listof String)))
              ([acc : (Listof (Pairof (U String #f) (Listof String)))
                      '()])
              ([t (in-list title*)]
               [r (in-list rktd*)])
      (cond
       ;[(and t ((inst assoc (U #f String) (Listof String)) t acc))
       [(and t (assoc t acc))
        (for/list ([t+r (in-list acc)])
          (define hd (car t+r))
          (if (and (string? hd) (string=? hd t))
            (list* t r (cdr t+r))
            t+r))]
       [else
        (cons (list t r) acc)]))
    (for/list ([t (in-list title*)] [r (in-list rktd*)])
      (list t r))))

;(: get-deathscore-pict (->* [(Listof String)] [#:tag String #:titles (U #f (Listof String))] Pict))
;(define (get-deathscore-pict rktd* #:tag [tag ""] #:titles [maybe-title* #f])
;  ;(define title+rktd* (zip-title* rktd* maybe-title* #:collapse? #f))
;  (define S*
;    (for/list : (Listof Summary)
;              ([d : String rktd*])
;      (from-rktd d)))
;  (car (make-plot death-plot S*)))

;; Create a pict, cache it for later use
(: get-new-lnm-pict (->* [(Listof String)] [#:tag String #:titles (U #f (Listof String))] Pict))
(define (get-new-lnm-pict rktd* #:tag [tag ""] #:titles [maybe-title* #f])
  (define title+rktd* (zip-title* rktd* maybe-title*))
  ;; Align all picts vertically first
  (define columns : (Listof Pict)
    (or (for/fold : (U #f (Listof Pict))
              ([prev* : (U #f (Listof Pict)) #f])
              ([title+rktd : (Pairof (U #f String) (Listof String)) (in-list title+rktd*)])
      (define title (car title+rktd))
      (define rktd* (cdr title+rktd))
      (define pict* (file->pict* rktd* #:title title))
      (if prev*
          ;; Right-align the old picts with the new ones
          (for/list : (Listof Pict)
                    ([old (in-list prev*)]
                     [new (in-list pict*)])
            (vr-append GRAPH-VSPACE old new))
          ;; Generate titles. Be careful aligning the summary row
          (if (PARAM-L-LABELS?)
            (cons (car pict*)
             (for/list : (Listof Pict)
                       ([l-str (in-list (make-L-title*))]
                        [new (in-list (cdr pict*))])
               (vc-append TITLE-VSPACE (text l-str TITLE-STYLE TITLE-SIZE) new)))
            pict*)))
         (error 'invariant)))
  (define columns/stats
    (if (PARAM-STATS?) columns (cdr columns)))
  ;; Paste the columns together, insert a little extra space to make up for
  ;;  the missing title in the first column
  (define pict0 : Pict
    (or (for/fold : (U #f Pict)
              ([prev-pict : (U #f Pict) #f])
              ([c columns/stats])
      (if prev-pict
          (hc-append GRAPH-HSPACE prev-pict c)
          (if (PARAM-L-LABELS?)
            (vc-append TITLE-VSPACE (blank 0 10) c)
            c)))
        (error 'invariant)))
  (define pict/legend
    (if (PARAM-LEGEND?)
      (vc-append (* 1 TITLE-VSPACE)
        pict0
        (make-legend))
      pict0))
  (cache-pict pict/legend rktd* tag)
  pict/legend)

;; =============================================================================
(define-syntax-rule (reads l)
  (with-input-from-string (assert l string?) read))

  (require
    racket/cmdline
    (only-in racket/list first last))
  (require/typed "scripts/show-pict.rkt"
   [pict->png (-> Pict Path-String Boolean)])

(: render-lnm (->* [(Vectorof String)] [#:extra (Option String)] Any))
(define (render-lnm vec #:extra [extra #f])

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
     [(and (file-exists? fname)
           (regexp-match? #rx"\\.rktd$" fname))
      #t]
     [else
      (printf "Skipping invalid file '~a'\n" fname)
      #f]))

  (define *output* (make-parameter "./output.png"))
  (: *tag* (Parameterof (U #f String)))
  (define *tag* (make-parameter #f))
  (command-line
   #:program "view-pict"
   #:argv vec
   #:once-each
   [("-o" "--output") o-param
    "Location to save results" (*output* (cast o-param String))]
   [("-p" "--path" "--paths")
    "If set, draw a line for lattice paths" (*show-paths?* #t)]
   [("-a" "--aggregate" "-d" "--death")
    "Combine all data into a single figure" (*aggregate* 'avg-prop)]
   [("--tag") tag "caching tag" (*tag* (cast tag (U #f String)))]
   [("--legend") legend "#t/#f = show/hide legend" (PARAM-LEGEND? (assert (reads legend) boolean?))]
   [("--split") "Put each L in a new plot" (PARAM-SPLIT? #t)]
   [("--stats") stats "#t/#f = show/hide summary statistics" (PARAM-STATS? (assert (reads stats) boolean?))]
   [("--ltitle") ltitle "#t/#f = show/hide L labels above plots" (PARAM-L-LABELS? (assert (reads ltitle) boolean?))]
   [("--labels") lbl "#t/#f = show/hide axis labels" (PARAM-AXIS-LABELS? (assert (reads lbl) boolean?))]
   [("--cutoff") c "Set red line with a number in [0,1] (#f by default)"
                 (PARAM-CUTOFF (cast (reads c) (U Real #f)))]
   [("--pdf") "Plot derivative of L-N/M plot (instead of cumulative)" (PARAM-PDF? #t)]
   [("-N") n "Set line for N (#f by default)" (PARAM-N (cast (reads n) (U Index #f)))]
   [("-M") m "Set line for M (#f by default)" (PARAM-M (cast (reads m) (U Index #f)))]
   [("-L") l "Set L values, may be natural, (listof natural) or (listof (list natural pen-style))"
             (let ([val (reads l)])
               (cond
                [(exact-nonnegative-integer? val)
                 (PARAM-L val)]
                [(and (list? val) (andmap exact-nonnegative-integer? val))
                 (PARAM-L (cast val (Listof Index)))]
                [else
                 (PARAM-L (cast val (Listof (List Index Plot-Pen-Style))))]))]
   #:args FNAME*
   ;; -- Filter valid arguments, assert that we got anything to render
   (define arg* (filter-valid-filenames FNAME*))
   (when (null? arg*)
     (raise-user-error "Usage: render-lnm.rkt DATA.rktd ..."))
   ;; -- Create a pict
   (define P
     (parameterize ([extra-summary extra])
       (data->pict
         #:tag (let ([tag (*tag*)])
                 (or tag
                     (format "cmdline~a" (if (*show-paths?*) "-path" ""))))
         (for/list : (Listof (List String String))
                   ([fname (in-list arg*)])
           (list (path->project-name (string->path fname)) fname)))))
   ;; TODO: pict->png should be typed, and defined in this module.
   P
   #;
   (pict->png P (*output*))))

(module+ main
  (render-lnm (current-command-line-arguments)))
