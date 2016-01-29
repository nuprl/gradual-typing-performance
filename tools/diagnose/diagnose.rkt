#lang racket/base

;; TODO
;; - scatterplot
;; - remove hline when row removed
;; - plot on success, gracefull (return boolean, use that)
;; - infer row, if only one
;; - label x-min
;; - show ranges
;; - normalize on same scale

;; Diagnosis tool.
;; Find the boundaries in common with bad configurations


(provide
  dataset?
)

(require
  (only-in racket/file file->value)
  racket/list
  racket/set
  racket/sequence
  racket/string
  plot/no-gui
)

;; =============================================================================
;; === Type Definitions

(struct dataset (
 src     ; Path-String
 config* ; (Listof Bitstring)
 row*    ; (Listof Row)
) #:transparent )
; (define-type Row (List Symbol Unit Data))
; (define-type Unit (U 'ms 'count 'bytes))
; (define-type Q+ Exact-Nonnegative-Rational)
; (define-type Data (Listof Q+))
; (define-type Dataset dataset)

; (: empty-dataset (-> Dataset))
(define (empty-dataset) (dataset "." '() '()))

;; =============================================================================
;; === Parameters & Configuration

; (: *active-rows* (Parameterof (Listof Symbol)))
(define *active-rows* (make-parameter '()))

; (: *active-hlines* (Parameterof (Listof (List Symbol Q+ BoundType))))
(define *active-hlines* (make-parameter '()))

; (: *active-predicates* (Parameterof (Listof Predicate)))
(define *active-predicates* (make-parameter '()))

; (: *current-predicates* (Parameterof (Listof Predicate)))
(define *current-predicates* (make-parameter '()))

; (: *current-dataset* (Parameterof Dataset))
(define *current-dataset* (make-parameter (empty-dataset)))

(unless (directory-exists? "./compiled") (make-directory "./compiled"))
(define PLOT-DIR "./compiled/_plot")
(define DEFAULT-PLOT (string-append PLOT-DIR "/tmp.png"))
(define CONFIG-FILE ".diagnose")

(rectangle-alpha 0.5)
(plot-width (* 3 (plot-width)))
(plot-height (* 2 (plot-height)))

; (define-type Predicate (List Symbol (-> Bitstring Boolean)))
; (define-type BoundType (U 'upper 'lower))

;; =============================================================================
;; === Commands

(define (add-row . sym*)
  (define r* (current-rows))
  (*active-rows*
    (append
      (for/list ([s (in-list sym*)] #:when (memq s r*)) s)
      (*active-rows*)))
  (plot))

;; Add a new hline, use to filter configurations
;; (: add-hline (->* [Symbol Real] [(U (U 'upper 'lower) Boolean)] Void))
(define (add-hline r n [bnd-type 'lower])
  (if (memq r (current-rows))
      (cond
       [(exact-nonnegative-integer? n)
        ;;
        (*active-hlines* (cons (list r n bnd-type) (*active-hlines*)))
        (plot)]
       [(and (< 0 n) (< n 1))
        ;; Assume n is a proportion
        (define M (max* (row->data r)))
        (*active-hlines* (cons (list r (round (* n M)) bnd-type) (*active-hlines*)))
        (plot)]
       [else
        (printf "Invalid input '~a', expected Natural or proportion" n)])
      (printf "Invalid row '~a'\n" r)))

(define (add-predicate s [fn #f])
  (cond
    [fn
     (cond
       [(not (symbol? s))
        (printf "Error: need symbol to identify new predicate '~a'\n" fn)]
       [(predicate-exists? s)
        (printf "Error: already have predicate with name '~a'. Pick another name." s)]
       [else
        (*current-predicates* (cons (list s fn) (*current-predicates*)))
        (*active-predicates* (cons (list s fn) (*active-predicates*)))])]
    [(symbol? s)
      (if (not (predicate-exists? s))
          (printf "Error: predicate '~a' does not exist, cannot add" s)
          (*active-predicates* (cons (list s (symbol->predicate s)) (*active-predicates*))))]
    [(or (string? s) (regexp? s))
     (let ([r (predicate*-matching s)])
       (if (null? r)
           (printf "Warning: pattern '~a' did not match any predicates" s)
           (*active-predicates* (append r (*active-predicates*)))))]
    [else
     (printf "Error: don't know what to do with '~a'\n" s)]))

(define (covering [k 3])
  (or
    (for/first ([s (covering* k)]) (displayln s))
    (displayln "No covering")))

(define (all-coverings [k 3])
  (or
    (for/fold ([acc #f])
              ([s (covering* k)])
      (printf "[[~a predicates]]\n" (length s))
      (for ([x s])
        (display "  ")
        (if (< (length (cdr x)) 200)
          (displayln x)
          (displayln (car x))))
      #t)
    (displayln "No covering")))

(define (count-configs)
  (displayln (length (current-configurations/hline))))

(define (covering* k)
  (covering*/configs k (current-configurations/hline)))

(define (covering*/configs k c*)
  (define N (length c*))
  (sequence-filter
   (lambda (x) x)
   (sequence-map
    (lambda (p*)
      (let ([sym+cfg* (for/list ([p (in-list p*)])
                        (cons (car p)
                              (filter (cadr p) c*)))])
        (and
         ;; All predicates get some config
         (for/and ([sym+cfg (in-list sym+cfg*)])
           (not (null? (cdr sym+cfg))))
         ;; Combined predicates cover all configs
         (= N (set-count
               (for*/set ([sym+cfg (in-list sym+cfg*)]
                          [c (in-list (cdr sym+cfg))])
                 c)))
         sym+cfg*)))
    (apply in-sequences
           (for/list ([i (in-range k)])
             (in-combinations (*active-predicates*) (+ i 1)))))))

(define (data)
  (cond
   [(null? (*active-rows*))
    (printf "Error: cannot print data for zero active rows\n")]
   [else
    (define r* (*active-rows*))
    (define c* (current-configurations))
    (define dest (data-destination r*))
    (with-output-to-file dest #:exists 'replace
      (lambda ()
        (display ";; ")
        (displayln (current-rows))
        (writeln (apply map list c* (map row->data r*)))))
    (printf "Saved data to '~a'\n" dest)]))

(define (plot [save? #f])
  (cond
   [(null? (*active-rows*))
    (printf "Error: cannot make plot for zero active rows\n")]
   [(null? (cdr (*active-rows*)))
    (plot-single save? (car (*active-rows*)))]
   [else
    (plot-multi save? (*active-rows*))]))

(define (remove-hline r)
  (*active-hlines*
    (for/list ([r+n (in-list (*active-hlines*))]
               #:when (not (eq? (car r+n) r)))
      r+n))
  (plot))

(define (remove-predicate s)
  (define pred-eq?
    (cond
     [(or (string? s) (regexp? s))
      (lambda (sym) (regexp-match? s (symbol->string sym)))]
     [else
      (lambda (sym) (eq? s sym))]))
  (*active-predicates*
    (for/list ([s+p (in-list (*active-predicates*))]
               #:when (not (pred-eq? (car s+p))))
      s+p)))

(define (remove-row . sym*)
  (*active-rows* (filter (lambda (r) (not (memq r sym*))) (*active-rows*)))
  (plot))

(define (reset)
  (reset-rows)
  (reset-hlines)
  (reset-predicates))

(define (reset-rows)
  (*active-rows* '()))

(define (reset-hlines)
  (*active-hlines* '()))

(define (reset-predicates)
  (*active-predicates* '()))

(define (load-active)
  (when (file-exists? CONFIG-FILE)
    (with-handlers ([exn:fail:filesystem?
                     (lambda (e) (printf "Error reading config file '~a'" CONFIG-FILE))])
      (load-from-file CONFIG-FILE
                      *active-rows*
                      *active-hlines*))))

(define-syntax-rule (load-from-file fname id* ...)
  (let ([v (file->value fname)])
    (id* (cadr (or (assq 'id* v) '(() ())))) ...))

(define (save-active)
  (save-to-file CONFIG-FILE
                *active-rows*
                *active-hlines*
                #;*active-predicates*))

(define-syntax-rule (save-to-file fname id* ...)
  (with-output-to-file fname #:exists 'replace
    (lambda ()
      (writeln
       (list
        (list 'id* (id*)) ...)))))

;; Count & print the currently selected configurations
; (: show-configs (-> Void))
(define (show-configs)
  (printf "(~a total)\n"
          (for/fold ([acc 0])
                    ([c (in-list (current-configurations/hline))])
            (displayln c)
            (+ acc 1))))

(define (show-predicates)
  (for ([s+p (in-list (*current-predicates*))])
    (displayln (car s+p))))

(define (show-rows)
  (displayln (current-rows)))

(define (help)
  (displayln "(add-row r* ...)       : Activate rows `r* ...`")
  (displayln "(add-hline r n)        : Add hline for row `r`. Optional third argument is (or/c 'upper 'lower).")
  (displayln "(add-predicate s)      : Active all predicates matching a symbol or regexp")
  (displayln "(add-predicate s f)    : Create and activate a predicate `f`, associated with symbol `s`")
  (displayln "---")
  (displayln "(covering)             : Find a covering for the current configs")
  (displayln "(data)                 : Print active dataset to FILE.png")
  (displayln "(plot)                 : Plot the current rows to FILE.png")
  (displayln "---")
  (displayln "(load-active)          : ")
  (displayln "(save-active)          : ")
  (displayln "(reset)                : Deactivate all rows and hlines")
  (displayln "(remove-hline r n)    : Remove hline for row `r`")
  (displayln "(remove-row r* ...)    : Deactivate rows `r* ...`")
  (displayln "(remove-predicate r* ...)   : Deactivate predicates")
  (displayln "---")
  (displayln "(count-configs)        : Count all configs above cutoff lines")
  (displayln "(show-configs)         : Print all configs above cutoff lines")
  (displayln "(show-predicates)      : Print the available predicates")
  (displayln "(show-rows)            : Print the available rows")
  (displayln "---")
  (displayln "(*active-rows*)        : Print the active rows")
  (displayln "(*active-hlines*)      : Print the active hlines")
  (displayln "(*active-predicates*)  : Print the active predicates")
  (displayln "(*current-predicates*) : Print all known predicates")
)

;; =============================================================================
;; === Support

(define (max* x*)
  (when (null? x*)
    (error 'max* "Empty list"))
  (for/fold ([M (car x*)])
            ([x (in-list (cdr x*))])
    (max M x)))

(define (min* x*)
  (when (null? x*)
    (error 'min* "Empty list"))
  (for/fold ([M (car x*)])
            ([x (in-list (cdr x*))])
    (min M x)))

;; (: lo+hi->y-ivl (->* [(Pairof (Option Natural) (Option Natural))] [#:lo Natural #:hi Natural] Interval))
(define (lo+hi->y-ivl lo+hi #:lo [lo-default 0] #:hi [hi-default 1])
  (cond
   [(and (not (car lo+hi)) (cdr lo+hi))
    (ivl lo-default (cdr lo+hi))]
   [(and (car lo+hi) (not (cdr lo+hi)))
    (ivl (car lo+hi) hi-default)]
   [else
    (error 'lo+hi->y-ivl (format "Invalid lo+hi value '~a'" lo+hi))]))

; (: plot-multi (-> Boolean (Listof Symbol) Void))
(define (plot-multi save? row*)
  (define-values [title long-dest] (plot-destination row*))
  (define dest (if save? long-dest DEFAULT-PLOT))
  (define c* (current-configurations))
  (define x-ivl (ivl (first c*) (last c*)))
  (plot-file
    (apply append
      (for/list ([r (in-list row*)]
                 [color (in-naturals 1)])
        (define d* (row->data r))
        (define offset (min* d*))
        (define M (let ([m (max* d*)]) (if (zero? m) 1 (- m offset))))
        (cons
          (points (map list c* (map (lambda (d) (/ (- d offset) M)) d*))
                  #:color color
                  #:fill-color color
                  #:label (symbol->string r))
          (let ([lo+hi* (row->lo+hi* r)]
                [scale (lambda (l+h)
                         (cons (if (car l+h) (/ (- (car l+h) offset) M) #f)
                               (if (cdr l+h) (/ (- (cdr l+h) offset) M) #f)))])
            (if lo+hi*
                (for/list ([l+h (in-list lo+hi*)])
                  (rectangles (list (vector x-ivl (lo+hi->y-ivl (scale l+h)))) #:color color))
                '())))))
    dest
    'png
    #:title title
    #:x-label "configuration"
    #:y-label "proportion (relative to worst config)")
  (printf "Saved plot to '~a'\n" dest))

; (: plot-single (-> Boolean Symbol Void))
(define (plot-single save? r)
  (define-values [title long-dest] (plot-destination r))
  (define dest (if save? long-dest DEFAULT-PLOT))
  (define c* (current-configurations))
  (define d* (map list c* (row->data r)))
  (define xmax (for/fold ([acc 0]) ([c+d (in-list d*)]) (max acc (cadr c+d))))
  (plot-file
   (cons
    (points d*
            #:color 1
            #:fill-color 1
            #:sym 'fullcircle
            #:label (symbol->string r))
    (let ([lo+hi* (row->lo+hi* r)]
          [x-ivl (ivl (first c*) (last c*))])
      (if lo+hi*
          (for/list ([l+h (in-list lo+hi*)]
                     [c (in-naturals 1)])
            (rectangles (list (vector x-ivl (lo+hi->y-ivl l+h #:hi xmax))) #:color c))
          '())))
   dest
   'png
   #:title title
   #:x-label "configuration"
   #:y-label (format "~a (~a)" r (row->unit r)))
  (printf "Saved plot to '~a'\n" dest))

(define (data-destination r*)
  (string-append (tmp-destination r*) ".rktd"))

; (: plot-destination (-> (U Symbol (Listof Symbol)) (Values String Path-String)))
(define (plot-destination r*)
  (define title (format "~a : Config vs. ~a" (strip-directory (current-project-name)) r*))
  (values title (string-append (tmp-destination r*) ".png")))

(define (strip-directory str)
  (last (string-split str "/")))

(define (tmp-destination r*)
  (unless (directory-exists? PLOT-DIR) (make-directory PLOT-DIR))
  (format "~a/~a-config-vs-~a"
          PLOT-DIR
          (strip-directory (current-project-name))
          (if (list? r*)
              (string-join (map symbol->string r*) "-")
              r*)))

; (: path-string->dataset (-> Path-String (U #f Dataset)))
(define (path-string->dataset ps)
  (define d (with-handlers ([exn:fail:filesystem? (lambda (e) #f)])
              (file->value ps)))
  (and (list? d)
       (andmap list? d)
       (andmap symbol? (map car d))
       (eq? 'config (caar d))
       (let ([len (length (cddr (car d)))])
         (andmap (lambda (r) (= len (length r))) (map cdddr (cdr d))))
       (dataset
         ps
         (second (car d))
         (cdr d))))

; (: row-ref (-> Dataset Symbol (U Data #f)))
(define (row-ref D sym)
  (for/first ;: (U Data #f)
             ([r (in-list (dataset-row* D))]
              #:when (eq? (car r) sym))
    r))

; (: row->unit (-> Symbol (U #f Symbol)))
(define (row->unit sym)
  (let ([r (row-ref (*current-dataset*) sym)])
    (and r (cadr r))))

(define (row->data sym)
  (let ([r (row-ref (*current-dataset*) sym)])
    (and r (third r))))

; (: row->predicate* (-> Symbol (Listof (Pairof (Option Natural) (Option Natural)))))
(define (row->predicate* sym)
  (for/list ([r+f+b (in-list (*active-hlines*))]
              #:when (eq? sym (car r+f+b)))
    (let ([v (cadr r+f+b)])
      (case (caddr r+f+b)
        [(upper) (lambda (n) (< n v))]
        [(lower) (lambda (n) (< v n))]
        [else (boundtype-error 'row->predicate r+f+b)]))))

;; (: row->lo+hi* (-> Symbol (Listof (Pairof (Option Natural) (Option Natural)))))
(define (row->lo+hi* sym)
  (for/list ([r+f+b (in-list (*active-hlines*))]
             #:when (eq? sym (car r+f+b)))
    (case (caddr r+f+b)
      [(upper) (cons #f (cadr r+f+b))]
      [(lower) (cons (cadr r+f+b) #f)]
      [else (boundtype-error 'row->lo+hi r+f+b)])))

(define-syntax-rule (boundtype-error sym val)
  (error sym (format "Unknown bound type '~a' in '~a'\n" (caddr val) val)))

(define (current-configurations)
  (map bitstring->nat (dataset-config* (*current-dataset*))))

(define (current-configurations/hline)
  (define D (*current-dataset*))
  (define row** (dataset-row* D))
  (define row-label* (map car row**))
  (define row-data** (map third row**))
  (let loop
            ([cfg* (dataset-config* D)]
             [d**   row-data**])
    (cond
     [(null? cfg*)
      '()]
     [(for/and ([lbl (in-list row-label*)]
                [d*  (in-list d**)])
        (let ([d (car d*)])
          (for/and ([f (in-list (row->predicate* lbl))])
            (f d))))
      ;; If this config is "<" all the filters, keep it
      (cons (car cfg*)
            (loop (cdr cfg*) (map cdr d**)))]
     [else
      (loop (cdr cfg*) (map cdr d**))])))

(define (current-project-name)
  (car (string-split (dataset-src (*current-dataset*)) "-")))

(define (current-rows)
  (map car (dataset-row* (*current-dataset*))))

(define (bitstring->nat str)
  (define L (string-length str))
  (for/sum ([n (in-range L)])
    (if (eq? #\1 (string-ref str (- L 1 n)))
      (expt 2 n)
      0)))

; (: load-dataset (-> Path-String (U Dataset EOF)))
(define (load-dataset ps)
  (let loop ;: Dataset
            ([ps ps]) ; (U Path-String EOF)
    (or (and (eof-object? ps) ps)
        (path-string->dataset ps)
        (loop (prompt/rx #rx"\\.rktd$"
                (format "Error loading dataset at '~a'. Please enter a new path-string:" ps))))))

;; =============================================================================
;; === Predicates

(define (predicate-exists? s)
  (if (symbol->predicate s)
    #t
    #f))

(define (predicate*-matching pat)
  (for/list ([s+p (in-list (*current-predicates*))]
             #:when (regexp-match? pat (symbol->string (car s+p))))
    s+p))

(define (symbol->predicate s)
  (for/first ([s+p (in-list (*current-predicates*))]
              #:when (eq? s (car s+p)))
    (cadr s+p)))

(define ((typed-at? n) cfg)
  (eq? #\1 (string-ref cfg n)))

(define ((untyped-at? n) cfg)
  (eq? #\0 (string-ref cfg n)))

(define ((has-typed? n) cfg)
  (= n (for/sum ([c (in-string cfg)])
         (if (eq? #\1 c) 1 0))))

(define ((has-untyped? n) cfg)
  (= n (for/sum ([c (in-string cfg)])
         (if (eq? #\0 c) 1 0))))

(define (boundary-at? n m)
  (let ([tn? (typed-at? n)]
        [tm? (typed-at? m)]
        [un? (untyped-at? n)]
        [um? (untyped-at? m)])
    (lambda (cfg)
      (or (and (tn? cfg) (um? cfg))
          (and (un? cfg) (tm? cfg))))))

(define (no-boundary-at? n m)
  (let ([tn? (typed-at? n)]
        [tm? (typed-at? m)]
        [un? (untyped-at? n)]
        [um? (untyped-at? m)])
    (lambda (cfg)
      (or (and (tn? cfg) (tm? cfg))
          (and (un? cfg) (um? cfg))))))

; (: make-has-typed? (-> Natural (Listof Predicate)))
(define (make-has-typed? n)
  (for/list ([i (in-range n)])
    (list
      (string->symbol (format "has-typed-~a" i))
      (has-typed? i))))

; (: make-has-untyped? (-> Natural (Listof Predicate)))
(define (make-has-untyped? n)
  (for/list ([i (in-range n)])
    (list
      (string->symbol (format "has-untyped-~a" i))
      (has-untyped? i))))

(define (make-untyped-at? n)
  (for/list ([i (in-range n)])
    (list
      (string->symbol (format "untyped-at-~a" i))
      (untyped-at? i))))

(define (make-typed-at? n)
  (for/list ([i (in-range n)])
    (list
      (string->symbol (format "typed-at-~a" i))
      (typed-at? i))))

(define (make-boundary-at? n)
  (for/list ([i* (in-combinations (range n) 2)])
    (list
     (string->symbol (format "boundary-at-~a/~a" (car i*) (cadr i*)))
     (boundary-at? (car i*) (cadr i*)))))

(define (make-no-boundary-at? n)
  (for/list ([i* (in-combinations (range n) 2)])
    (list (string->symbol (format "no-boundary-at-~a/~a" (car i*) (cadr i*)))
          (no-boundary-at? (car i*) (cadr i*)))))

(define (initialize-predicates)
  (define num-modules
    (string-length (car (dataset-config* (*current-dataset*)))))
  (*current-predicates*
    (append
      (make-boundary-at? num-modules)
      (make-no-boundary-at? num-modules)
      (make-has-untyped? num-modules)
      (make-untyped-at? num-modules)
      (make-has-typed? num-modules)
      (make-typed-at? num-modules)))
  (*active-predicates* (*current-predicates*)))
  ;(add-predicate "^boundary.*"))

;; =============================================================================
;; === Prompting

; (: prompt/rx (-> Regexp String (U String EOF)))
(define (prompt/rx r str)
  (let ([response (prompt str)])
    (if (or (eof-object? response) (regexp-match? r response))
      response
      (begin (printf "Error: expected a response matching '~a'. Try again." r)
             (prompt/rx r str)))))

; (: prompt (-> String (U EOF String)))
(define (prompt str)
  (displayln str)
  (display "> ")
  (flush-output)
  (read-line))

;; =============================================================================
;; === Main

;; Another idea: module->namespace on a "commands" submodule
(define-namespace-anchor nsa)

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "gtp-diagnose"
   #:args (filename)
   ; (: ds (U EOF Dataset))
   (printf "[INFO] Loading dataset for '~a'...\n" filename)
   (define ds (load-dataset filename))
   (unless (eof-object? ds)
     (parameterize ([current-namespace (namespace-anchor->namespace nsa)]
                    [*current-dataset* ds])
       (printf "[INFO] Initializing REPL...\n")
       (initialize-predicates)
       (load-active)
       (namespace-require 'xrepl)
       (read-eval-print-loop))))
)
