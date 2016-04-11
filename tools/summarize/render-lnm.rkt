#lang typed/racket/base

;; 1. get working
;;   - pdf use points
;;   - aggregate
;; 2. clean code
;; 3. document

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
)

;; -----------------------------------------------------------------------------

(require
 (only-in racket/file file->value)
 (only-in racket/port with-input-from-string open-output-nowhere)
 (only-in racket/math exact-round)
 racket/list
 racket/string
 gtp-summarize/lnm-parameters
 gtp-summarize/lnm-plot
 gtp-summarize/summary
 ;plot/typed/no-gui
 plot/typed/utils
 racket/cmdline
 typed/pict
 typed/racket/class
)

(define-type Pict pict)

(require/typed racket/serialize
  [serialize (-> Pict Any)]
  [deserialize (-> Any Pict)])

;; =============================================================================

(defparam *GRAPH-HSPACE* Positive-Integer 10)
(defparam *GRAPH-VSPACE* Positive-Integer 20)
(defparam *TITLE-VSPACE* Positive-Integer (assert (exact-round (/ (*GRAPH-VSPACE*) 2)) positive?))

(define DEBUG #t)
(define-syntax-rule (debug msg arg* ...)
  (when DEBUG (printf msg arg* ...) (newline)))

;; =============================================================================

;(: assert-false/L (-> (Parameterof Any) Void))
(define-syntax-rule (assert-false/L PARAM)
  (when (PARAM)
    (raise-user-error 'render-lnm
      (format "Cannot make L-titles with parameter ~a is non-#f" (object-name PARAM)))))

;; Important parameters.
;; If these are changed, ignore cached pict
(: current-param-tag* (-> (Listof String)))
(define (current-param-tag*)
  (for/list : (Listof String)
      ([x (in-list (list (*PDF?*) (*SINGLE-PLOT?*) (*MAKE-TABLE?*)
                         (*AXIS-LABELS?*) (*L-LABELS?*) (*LINE-LABELS?*) (*TITLE?*)
                         (*LEGEND?*) (*SHOW-PATHS?*) (*LOG-TRANSFORM?*)
                         (*HISTOGRAM?*) (*MAX-OVERHEAD*) (*NUM-SAMPLES*)
                         (*PLOT-WIDTH*) (*PLOT-HEIGHT*) (*Y-STYLE*)
                         (*AGGREGATE*) (*N*) (*M*) (*L*)))])
    (format "~a" x)))

(: make-L-title* (-> (Listof String)))
(define (make-L-title*)
  (assert-false/L *AGGREGATE*)
  (assert-false/L *SHOW-PATHS?*)
  (assert-false/L *SINGLE-PLOT?*)
  (assert-false/L *PDF?*)
  ;; --
  (define L (*L*))
  (cond
   [(not (list? L))
    (list (format "L = ~a (steps)" L))]
   [else
    ;; Normalize L data to list of Naturala
    (: normL (Listof Natural))
    (define normL
      (for/list : (Listof Natural)
                ([x : (U Natural (List Natural Plot-Pen-Style)) (in-list L)])
        (if (list? x)
          (car x)
          x)))
    (define num-L (length normL))
    (: l-index->string (-> Integer String))
    (define (l-index->string i)
      (cond [(zero? i)
             (format "L = ~a" i)]
            [(= num-L i)
             (format "\t~a  (steps)" i)]
            [else
             (number->string i)]))
    (for/list : (Listof String)
              ([x (in-list normL)])
      (l-index->string x))]))

(: make-legend (-> Pict))
(define (make-legend)
  ;; VSHIM separates 2 rows in the legend
  (define VSHIM (/ (*TITLE-VSPACE*) 3))
  (: mytext (->* (String) ((U #f 'italic)) Pict))
  (define (mytext str [mystyle #f])
    (text str
      (if mystyle
        ((inst cons 'italic String) mystyle (*TITLE-FONT-FACE*))
        (*TITLE-FONT-FACE*))
      (assert (+ 1 (*TABLE-FONT-SIZE*)) index?)))
  ;; TODO spacing is sometimes wrong... recompiling fixes
  ;; but otherwise this looks okay
  (hc-append (* 6 (*GRAPH-HSPACE*))
    (vl-append VSHIM
     (mytext "x-axis: overhead")
     (mytext "y-axis: # configs"))
    (vl-append VSHIM
     (hc-append 0
       (colorize (mytext "red") "orangered")
       (let ([pc (*CUTOFF-PROPORTION*)])
         (if pc
           (mytext (format " line: ~a% of configs." (round (* 100 pc))))
           (blank))))
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
       (mytext (format "=~a" (*N*))))
     (hc-append 0
       (colorize (mytext "yellow") "goldenrod")
       (mytext " line: ")
       (mytext "M" 'italic)
       (mytext (format "=~a" (*M*)))))))

;; -----------------------------------------------------------------------------

;; Try to read a cached pict, fall back to making a new one.
(: data->pict (->* [(Listof (List String String))] [#:tag String] Pict))
(define (data->pict data* #:tag [tag ""])
  (define title* (for/list : (Listof String) ([x (in-list data*)]) (car x)))
  (define rktd* (for/list : (Listof String) ([x (in-list data*)]) (cadr x)))
  (or (get-cached rktd* #:tag tag)
      (case (*AGGREGATE*)
       [(#t #f) ;; boolean
        (get-new-lnm-pict rktd* #:tag tag #:titles title*)]
       [(mean)
        (get-deathscore-pict rktd* #:tag tag #:titles title*)]
       [else
        (raise-user-error 'render-lnm "Unknown aggregation method" (*AGGREGATE*))])))

;; Create a summary and L-N/M picts for a data file.
(: file->pict* (->* [(Listof String) #:title (U String #f)] (Listof Pict)))
(define (file->pict* data-file* #:title title)
  (define S* (for/list : (Listof Summary) ([d : String data-file*]) (from-rktd d)))
  ;; If we have only 1 summary object, we can tabulate stats
  (define S-tbl : Pict
    (if (and (not (null? S*)) (null? (cdr S*)) (*MAKE-TABLE?*))
      (let ([p (summary->pict (car S*)
                              #:title title
                              #:font-face (*TABLE-FONT-FACE*)
                              #:font-size (*TABLE-FONT-SIZE*)
                              #:N (assert (or (*N*) (raise-user-error 'render-lnm "Need a value for N, set the *N* parameter")) index?)
                              #:M (assert (or (*M*) (raise-user-error 'render-lnm "Need a value for M, set the *M* parameter")) index?)
                              #:width (* 0.6 (*PLOT-WIDTH*))
                              #:height (*PLOT-HEIGHT*))])
        (vc-append 0 p (blank 0 (- (*PLOT-HEIGHT*) (pict-height p)))))
      (blank 0 0)))
  (define L-pict* : (Listof Pict)
    ;; TODO there's only a 3-character difference between the branches...
    ;;  I tried making plot-fn = (if SHOW-PATHS? path-plot lnm-plot),
    ;;  but a U-type can't be applied!
    (parameterize ([*PLOT-FONT-SIZE* 6])
      (if (*SHOW-PATHS?*)
        (path-plot S*)
        (lnm-plot S*))))
  (cons S-tbl L-pict*))

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

(: ensure-dir (-> String Void))
(define (ensure-dir path)
  (unless (directory-exists? path)
    (make-directory path)))

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

(: zip-title* (->* [(Listof String) (U #f (Listof String))]
                   [#:collapse? Boolean]
                   (Listof (Pairof (U #f String) (Listof String)))))
(define (zip-title* rktd* maybe-title* #:collapse? [collapse? #f])
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
  (if collapse?
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
        (cons (list t r) acc)]))
    (for/list ([t (in-list title*)] [r (in-list rktd*)])
      (list t r))))

(: get-deathscore-pict (->* [(Listof String)] [#:tag String #:titles (U #f (Listof String))] Pict))
(define (get-deathscore-pict rktd* #:tag [tag ""] #:titles [maybe-title* #f])
  ;(define title+rktd* (zip-title* rktd* maybe-title* #:collapse? #f))
  (raise-user-error 'render-lnm "cannot make death scores right now"))
;  (define S*
;    (for/list : (Listof Summary)
;              ([d : String rktd*])
;      (from-rktd d)))
;  (car (make-plot death-plot S*)))

;; Create a pict, cache it for later use
(: get-new-lnm-pict (->* [(Listof String)] [#:tag String #:titles (U #f (Listof String))] Pict))
(define (get-new-lnm-pict rktd* #:tag [tag ""] #:titles [maybe-title* #f])
  (define title+rktd* (zip-title* rktd* maybe-title*
                        #:collapse? (assert (*AGGREGATE*) boolean?)))
  ;; Get all picts. (Each call to lnm-plot returns a list)
  (define pict**
    (for/list : (Listof (Listof Pict))
              ([title+rktd : (Pairof (Option String) (Listof String)) (in-list title+rktd*)])
      (file->pict* (cdr title+rktd) #:title (car title+rktd))))
  ;; Align all picts vertically first
  (define columns : (Listof Pict)
    (or (for/fold : (U #f (Listof Pict))
              ([prev* : (U #f (Listof Pict)) #f])
              ([pict* : (Listof Pict) (in-list pict**)])
      (if prev*
          ;; Right-align the old picts with the new ones
          (for/list : (Listof Pict)
                    ([old (in-list prev*)]
                     [new (in-list pict*)])
            (vr-append (*GRAPH-VSPACE*) old new))
          ;; Generate titles. Be careful aligning the summary row
          (if (*L-LABELS?*)
            (cons (car pict*)
             (for/list : (Listof Pict)
                       ([l-str (in-list (make-L-title*))]
                        [new (in-list (cdr pict*))])
               (vc-append (*TITLE-VSPACE*)
                          (text l-str
                                (*TITLE-FONT-FACE*)
                                (assert (*TITLE-FONT-SIZE*) index?))
                          new)))
            pict*)))
         (error 'invariant)))
  (define columns/stats
    (if (*MAKE-TABLE?*) columns (cdr columns)))
  ;; Paste the columns together, insert a little extra space to make up for
  ;;  the missing L-label in the first column
  (define pict0 : Pict
    (or
     (for/fold : (U #f Pict)
              ([prev-pict : (U #f Pict) #f])
              ([c columns/stats])
      (if prev-pict
          (hc-append (*GRAPH-HSPACE*) prev-pict c)
          (if (*L-LABELS?*)
            (vc-append (*TITLE-VSPACE*) (blank 0 10) c)
            c)))
     (error 'invariant)))
  (define pict/legend
    (if (*LEGEND?*)
      (vc-append (*TITLE-VSPACE*)
        pict0
        (make-legend))
      pict0))
  (cache-pict pict/legend rktd* tag)
  pict/legend)

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
    (printf "Skipping invalid file '~a'\n" fname)
    #f]))

(: pict->png (-> Pict Path-String Boolean))
(define (pict->png p path)
  (send (pict->bitmap p) save-file path 'png))

(: render-lnm (-> (Vectorof String) Any))
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
   [("-a" "--aggregate")
    "Combine all data into a single figure"
    (*AGGREGATE* #t)]
   ;; TODO enable aggregation
   ;[("-d" "--deathscore")
   ; sym
   ; "Create a deathscore, valid params: 'mean"
   ; (*AGGREGATE* sym)]
   [("--legend")
    legend
    "#t/#f = show/hide legend"
    (*LEGEND?* (assert (reads legend) boolean?))]
   [("--split")
    "Use different plot for each L"
    (*SINGLE-PLOT?* #f)]
   [("--single")
    "Put all L on the same plot"
    (*AGGREGATE* #t) ;; Also put all data for 1 benchmark in a single figure
    (*SINGLE-PLOT?* #t)]
   [("--hist" "--histogram" "-H")
    "Show CDF as a histogram"
    (*HISTOGRAM?* #t)]
   [("--make-table")
    "Create summary tables for each dataset"
    (*MAKE-TABLE?* #t)]
   [("--log")
    "Plot x-axis on a log scale"
    (*LOG-TRANSFORM?* #t)]
   [("--labels")
    lbl
    "Enable / Disable all labels"
    (let ([b (assert (reads lbl) boolean?)])
     (*AXIS-LABELS?* b)
     (*LINE-LABELS?* b)
     (*L-LABELS?*    b)
     (*TITLE?*       b))]
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
   [("--title")
    lbl
    "#t/#f = show/hide plot title"
    (*TITLE?* (assert (reads lbl) boolean?))]
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
    (*N* (assert (reads n) index?))]
   [("-M")
    m
    "Set line for M (#f by default)"
    (*M* (assert (reads m) index?))]
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
        #:tag (format "cmdline~a" (if (*SHOW-PATHS?*) "-path" ""))
        (for/list : (Listof (List String String))
                  ([fname (in-list arg*)])
          (list (path->project-name (string->path fname)) fname))))
   (pict->png P (*OUTPUT*))))

(module+ main (render-lnm (current-command-line-arguments)))

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
   ;; -- duplicate titles, but #:collapse is #f
   ['("a-1" "b-1") '("hi" "hi")
    '(("hi" "a-1") ("hi" "b-1"))])
  (check-equal?
   ;; -- duplicate titles, #:collapse is #t
   (zip-title* '("a-1" "b-1") '("hi" "hi") #:collapse? #t)
   '(("hi" "b-1" "a-1")))

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
