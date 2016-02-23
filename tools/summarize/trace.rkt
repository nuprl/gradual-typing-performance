#lang racket/base

;; TODO
;; - typed racket 

(provide
)

(require
  gtp-summarize/modulegraph
  glob
  (only-in racket/string string-replace string-prefix? string-split)
  (only-in racket/file delete-directory/files)
  (only-in racket/port copy-port)
  (only-in racket/system system process)
)

;; =============================================================================

(define TRACE-PREFIX 'trace:)
;; Prefix for indirected identifiers

(define TRACE-LOG-PREFIX "[TRACE]")

(define TRACE-TMP ".trace.tmp")
;; Temporary file to use for search/replace

(define *DIRECTORY?* (make-parameter #f))
;; If #t, assume input is a directory (instead of a project name)

(define *RKT-BIN* (make-parameter ""))
;; Location of Racket binaries

(define *TEX-OUT* (make-parameter "graph.tex"))
;(define *STD-OUT* (make-parameter #f))

(define *VERBOSE* (make-parameter #t))
(define-syntax-rule (debug str arg* ...)
  (when (*VERBOSE*)
    (display "[DEBUG] : ")
    (printf str arg* ...)
    (newline)))

;; =============================================================================

;; (define-type DynamicBoundary (List String String (Listof (Pairof Symbol Natural))))
;; (define-type Dynamic-ModuleGraph dynamic-modulegraph)

(struct dynamic-modulegraph (
  graph ;; ModuleGraph
  boundary* ;; (Listof DynamicBoundary)
  ;; TODO rename these fields
) #:transparent )

;; -----------------------------------------------------------------------------
;; Wrappers

(define (dmg->project-name dmg)
  (project-name (dynamic-modulegraph-graph dmg)))

(define (dmg->module-names dmg)
  (module-names (dynamic-modulegraph-graph dmg)))

(define (dmg->num-modules dmg)
  (length (modulegraph-adjlist (dynamic-modulegraph-graph dmg))))

(define dyn-boundary-to car)
(define dyn-boundary-from cadr)
(define dyn-boundary-provided+count* caddr)

(define (dyn-boundary*-ref b* to from)
  (for/or ([b (in-list b*)])
    (and (string=? to (dyn-boundary-to b))
         (string=? from (dyn-boundary-from b))
         (dyn-boundary-provided+count* b))))

(define (dyn-boundary*-ref/fail b* to from)
  (or (dyn-boundary*-ref b* to from)
      (raise-user-error 'trace "No info for boundary (~a ~a) in\n    ~a" to from b*)))

;; -----------------------------------------------------------------------------

(define (to->dyn-boundary* dmg to)
  (for/list ([b (in-list (dynamic-modulegraph-boundary* dmg))]
             #:when (string=? to (dyn-boundary-to b)))
    b))

;; Sum the total uses of all identifiers in this boundary
(define (dyn-boundary-count b)
  (for/sum ([p+c (in-list (dyn-boundary-provided+count* b))])
    (cdr p+c)))

;; Count all requires used by a module
(define (to->total-count DMG to)
  (for/sum ([b (in-list (to->dyn-boundary* DMG to))])
    (dyn-boundary-count b)))

;; =============================================================================

;; UGH gotta be a better / more consitent way of doing this
(define filename
  (let ([rx #rx"/?([^/]*)\\.rkt$"])
    (lambda (str)
      (cond
       [(regexp-match rx str) => cadr]
       [else (raise-user-error 'trace "Failed to get filename from '~a'" str)]))))

;; (: (-> Path-String Boolean))
(define (trace-directory-exists? dir)
  (define dir-str (path->string dir))
  (and (directory-exists? dir)
       (for/and ([u-file (in-glob (string-append dir-str "/../untyped/*.rkt"))])
         (= 2 (length (glob (string-append dir-str "/" (filename u-file) "*.rkt")))))))

;; Setup a directory for tracing.
;; - Copy all untyped files
;; - Make a compatibility interface for each untyped file
;; - Require compat. interfaces in each file
;; - Copy all 'both' files  (but these shouldn't matter)
;; (-> Path-String (Listof Boundary) Void)
(define (setup-trace-dir trace-dir boundary*)
  (make-directory trace-dir)
  (define trace-str (if (string? trace-dir) trace-dir (path->string trace-dir)))
  ;; -- Copy untyped files
  (copy-all (string-append trace-str "/../untyped") trace-str #:glob "*.rkt")
  ;; -- Make compatibility interfaces
  (define boundary-str (string-append trace-str "/boundary"))
  (make-directory boundary-str)
  (for ([b (in-list boundary*)])
    (define-values (to from provided*) (apply values b))
    (define boundary-filename (format "~a-~a-boundary.rkt" to from))
    (with-output-to-file (string-append boundary-str "/" boundary-filename)
      (lambda ()
        (printf "#lang racket/base\n")
        (printf "(provide (all-defined-out))\n")
        (printf "(require (for-syntax racket/base syntax/parse))\n")
        (printf "(require (prefix-in ~a \"../~a.rkt\"))\n" TRACE-PREFIX from)
        (newline)
        (for ([p (in-list provided*)])
          (define id (provided->symbol p))
          (define id/trace (format "~a~a" TRACE-PREFIX id))
          (define msg (format "~a\t~a\t~a\t~a" TRACE-LOG-PREFIX to from id))
          (printf "(define-syntax (~a stx)\n" id)
          (printf "  (syntax-parse stx\n")
          (printf "   [(_ arg* ...)\n")
          (printf "    (syntax/loc stx (begin\n")
          (printf "      (printf \"~a\\t~~a\\n\" '(arg* ...))\n" msg)
          (printf "      (~a arg* ...)))]\n" id/trace)
          (printf "   [id\n")
          (printf "    (syntax/loc stx (begin\n")
          (printf "      (displayln \"~a\\t#f\")\n" msg)
          (printf "      ~a))]))\n" id/trace))
        (void)))
    (string-replace/file (string-append trace-str "/" to ".rkt")
      (string-append from ".rkt")
      (string-append "boundary/" boundary-filename))
    (void))
  ;; -- Copy both files
  (copy-all (string-append trace-str "/../both") trace-str #:glob "*.rkt")
  trace-dir)

;; (: string-replace/file (-> Path-String String String Void))
(define (string-replace/file fname from to)
  (with-output-to-file TRACE-TMP #:exists 'replace
    (lambda ()
      (with-input-from-file fname
        (lambda ()
          (for ([ln (in-lines)])
            (displayln (string-replace ln from to)))))))
  (with-output-to-file fname #:exists 'replace
    (lambda ()
      (with-input-from-file TRACE-TMP
        (lambda ()
          (copy-port (current-input-port) (current-output-port))))))
  (delete-file TRACE-TMP))

;; (->* [Path-String Path-String] [#:glob String] Void)
(define (copy-all from-dir to-dir #:glob [glob "*"])
  (for ([b-file (in-glob (string-append from-dir "/" glob))])
    ;; Lazy here using system, because I don't want to get the filename from `b-file`
    (system (format "cp ~a ~a" b-file to-dir))))

;; Check that no existing trace directory exists, then build a new trace directory
(define (setup-trace-dir/check dir boundary*)
  (define trace-dir (build-path dir "trace"))
  (if (trace-directory-exists? trace-dir)
    (begin
      (debug "Found existing trace directory '~a'" trace-dir)
      trace-dir)
    (begin
      (when (directory-exists? trace-dir)
        (delete-directory/files trace-dir))
      (setup-trace-dir trace-dir boundary*))))

;; Create a hash from a list of boundaries.
;; Return the hash and an increment procedure.
;; (-> (Listof Boundary) (Values DynHash (-> String String Symbol Void)))
(define (boundary*->dyn-hash b*)
  (define H
    (for/hash ([b (in-list b*)])
      (define-values (to from provided*) (apply values b))
      (define p-hash
        (make-hasheq
          (for/list ([p (in-list provided*)]) (cons (provided->symbol p) 0))))
      (values (cons to from) p-hash)))
  (define (H++ to from p)
    (define H1 (hash-ref H (cons to from) (lambda () #f)))
    (define old-val (and H1 (hash-ref H1 p (lambda () #f))))
    (if (and H1 old-val)
      (hash-set! H1 p (+ old-val 1))
      (if old-val
        (printf "Warning: no match for keys {to : ~a} {from : ~a}\n" to from)
        (printf "Warning: no value for key {to : ~a} {from : ~a} {id : ~a}\n" to from p))))
  (values H H++))

;; (-> DynHash (Listof DynamicBoundary))
(define (dyn-hash->dyn-boundary* H)
  (for/list ([(to+from p*) (in-hash H)])
    (list (car to+from)
          (cdr to+from)
          (sort
            (for/list ([(p v) (in-hash p*)])
              (cons p v))
            symbol<?
            #:key car))))

;; Run a program, with tracing
;; (-> Path-String (Listof Boundary) (Listof DynamicBoundary))
(define (trace-run dir b*)
  (define dir-str (if (string? dir) dir (path->string dir)))
  (define main-file (string-append dir-str "/main.rkt"))
  (define-values (H H++) (boundary*->dyn-hash b*))
  ;; -- compile the project
  (unless (system (format "~araco make ~a" (*RKT-BIN*) main-file))
    (raise-user-error 'trace "Failed to compile ~a" main-file))
  ;; -- run a subprocess, collect output
  (define cmd (format "~aracket ~a" (*RKT-BIN*) main-file))
  (define-values (in out pid err check-status)
    (apply values
      (parameterize ([current-directory dir]) (process cmd))))
  (define num-lines (box 0))
  (define (subprocess-read)
    (for ([line (in-lines in)])
      (set-box! num-lines (+ 1 (unbox num-lines)))
      (cond
       [(string-prefix? line TRACE-LOG-PREFIX)
        (apply H++ (log->data line))]
       [else
        (void)])))
  ;; --- do the output collection
  (let loop ()
    (case (check-status 'status)
     [(running)
      (debug "Subprocess running, reading output so far")
      (subprocess-read)
      (loop)]
     [(done-ok)
      (subprocess-read)
      (debug "Subprocess finished cleanly. Produced ~a lines of output." (unbox num-lines))]
     [(done-error)
      (parameterize ([current-output-port (current-error-port)])
        (for ([line (in-lines err)]) (displayln line)))
      (raise-user-error 'trace "Subprocess '~a' exited with an error" cmd)]))
  ;; -- close pipe ports
  (close-input-port in)
  (close-output-port out)
  (close-input-port err)
  ;; -- immutable output
  (dyn-hash->dyn-boundary* H))

(define (log->data str)
  (define-values (tag to from id payload) (apply values (string-split str "\t")))
  (list to from (string->symbol id)))

;; (: directory->dmg (-> Path-String Dynamic-ModuleGraph))
(define (directory->dmg dir)
  (define MG (directory->modulegraph dir))
  (define B* (boundaries MG))
  (define trace-dir (setup-trace-dir/check dir B*))
  (define dyn-B* (trace-run trace-dir B*))
  (dynamic-modulegraph MG dyn-B*))

(define (project-name->dmg name)
  (directory->dmg (infer-project-dir name)))

;; Pretty-print a dynamic module graph to the current output port
;; (: print-dmg (-> DMG Void))
(define (print-dmg dmg)
  ;; Print title
  (define title (format "ModuleGraph for ~a:" (dmg->project-name dmg)))
  (displayln title)
  (displayln (make-string (string-length title) #\=))
  (newline)
  ;; For each module, print info about its requires
  (for ([to (in-list (dmg->module-names dmg))])
    (define b* (to->dyn-boundary* dmg to))
    (when (not (null? b*))
      (printf "Requires for '~a.rkt'\n" to)
      (define indent "  - ")
      (for ([b (in-list b*)])
        (printf "  - ~a.rkt : ~a\n" (dyn-boundary-from b) (dyn-boundary-count b)))
      (printf "Total: ~a\n" (to->total-count dmg to))
      (newline))))

;; (-> String Void)
(define (assert-rkt-bin r)
  (unless (and (file-exists? (string-append r "/racket"))
               (file-exists? (string-append r "/raco")))
    (raise-user-error 'trace "Directory '~a' does not have 'racket' and 'raco' executables" r)))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "gtp-trace"
   #:once-each
   [("--tex") tex "File to store .tex output to" (*TEX-OUT* tex)]
   [("-q" "--quiet") "Run quietly" (*VERBOSE* #f)]
   [("-r" "--racket") r "Run quietly" (and (assert-rkt-bin r)
                                           (*RKT-BIN* (string-append r "/")))]
   [("-d" "--directory") "Run from a specific directory, instead of by project name" (*DIRECTORY?* #t)]
   #:args (PROJECT-NAME)
   (define DMG
     (if (*DIRECTORY?*)
       (directory->dmg (build-path (current-directory) PROJECT-NAME))
       (project-name->dmg PROJECT-NAME)))
   (print-dmg DMG))
)

;; =============================================================================

(module+ test
  (*VERBOSE* #f)

  (require
    rackunit
    (only-in racket/file file->lines))

  ;; ---
  (define-syntax-rule (check-filename* [in out] ...)
    (begin
      (check-equal? (filename in) out) ...))
  (check-filename*
   ["foo/bar.rkt" "bar"]
   ["bar.rkt" "bar"]
   ["/foo/bar/baz/qux.rkt" "qux"]
   ["/////hi///bar.rkt" "bar"])

  (define-syntax-rule (check-string-replace/file line* search-str replace-str)
    (let ([TEST-FILE "test/search-and-replace.txt"])
      (with-output-to-file TEST-FILE #:exists 'replace
        (lambda () (map displayln line*)))
      (string-replace/file TEST-FILE search-str replace-str)
      (check-equal?
        (file->lines TEST-FILE)
        (map (lambda (ln) (string-replace ln search-str replace-str)) line*))
      (map delete-file (glob (string-append TEST-FILE "*")))
      (void)))

  (check-string-replace/file
    '("the" "quick" "brown fox" "jumped over" "the" "lazy" "d og")
    "d o"
    "yolo")

  (let ([provided* (list (list "toA" "fromA" (list (provided 'A1 #f #f)
                                                   (provided 'A2 #f #f)
                                                   (provided 'A3 #f #f)))
                         (list "toB" "fromB" (list (provided 'B1 #f #f))))])
    (let-values ([(H H++) (boundary*->dyn-hash provided*)])
      (check-equal? (hash-count H) 2)
      (check-equal?
        (hash-count (hash-ref H (cons "toA" "fromA") (lambda () (error 'testfailed))))
        3)
      ;; -- check value before & after incrementing
      (check-equal?
        (hash-ref
          (hash-ref H (cons "toB" "fromB") (lambda () (error 'testfailed)))
          'B1)
        0)
      (H++ "toB" "fromB" 'B1)
      (check-equal?
        (hash-ref
          (hash-ref H (cons "toB" "fromB") (lambda () (error 'testfailed)))
          'B1)
        1)
      ;; -- check dynhash->dynboundary
      (check-equal?
        (sort (dyn-hash->dyn-boundary* H) string<? #:key car)
        '(("toA" "fromA" ((A1 . 0) (A2 . 0) (A3 . 0)))
          ("toB" "fromB" ((B1 . 1)))))
      ))

  (define-syntax-rule (check-log->data* [in out] ...)
    (begin (check-equal? (log->data in) out) ...))
  (check-log->data*
   ["[TRACE]\tto\tfrom\tid\twhateverelse"
    '("to" "from" id)]
   ["A\tB\tC\tD\tE\n"
    '("B" "C" D)])

  ;; -- 
  (let ([b* '(("A" "B" ((DATA . 400001) (PORT . 1))))])
    (check-equal?
      (dyn-boundary*-ref/fail b* "A" "B")
      '((DATA . 400001) (PORT . 1)))
    (check-false
      (dyn-boundary*-ref b* "YO" "LO")))

  ;; -- end-to-end
  (define (check-dmg dir-name)
    (printf "[TEST] Building dynamic graph for '~a'... \n" dir-name)
    (define dmg (directory->dmg (build-path (current-directory) "test" dir-name)))
    (printf "[TEST] finished building dynamic graph for '~a'\n" dir-name)
    dmg)

  #;(let* ([DMG (check-dmg "sample_modulegraph_dir")]
         [B*  (dynamic-modulegraph-boundary* DMG)])
    (check-equal? (length B*) 4)
    (check-equal?
      (dyn-boundary*-ref/fail B* "main" "client")
      '((client . 1)))
    (check-equal?
      (dyn-boundary*-ref/fail B* "main" "server")
      '((server . 1)))
    (check-equal?
      (dyn-boundary*-ref/fail B* "server" "constants")
      '((DATA . 1) (PORT . 1)))
    (check-equal?
      (dyn-boundary*-ref/fail B* "client" "constants")
      '((DATA . 400001) (PORT . 1)))
  )

  #;(let* ([DMG (check-dmg "mini-morsecode")]
         [B* (dynamic-modulegraph-boundary* DMG)])
    ;; -- 20 because the first 4 words in `base/frequency-small.rktd`
    ;;    have 10 total characters & we use each of the words 2x
    (check-equal?
      (dyn-boundary*-ref/fail B* "morse-code-strings" "morse-code-table")
      '((char-table . 20)))
    ;; -- 8 because we make 2 calls in each of 4 iterations in `main.rkt`
    (check-equal?
      (dyn-boundary*-ref/fail B* "main" "morse-code-strings")
      '((string->morse . 8)))
    ;; -- all 0 except for string-levenshtein
    (check-equal?
      (dyn-boundary*-ref/fail B* "main" "levenshtein")
      '((levenshtein . 0) (list-levenshtein . 0) (list-levenshtein/eq . 0)
        (list-levenshtein/equal . 0) (list-levenshtein/eqv . 0)
        (list-levenshtein/predicate . 0) (string-levenshtein . 8)
        (vector-levenshtein . 0) (vector-levenshtein/eq . 0)
        (vector-levenshtein/equal . 0) (vector-levenshtein/eqv . 0)
        (vector-levenshtein/predicate . 0)
        (vector-levenshtein/predicate/get-scratch . 0))))

  #;(let* ([DMG (check-dmg "mini-zombie")]
         [B* (dynamic-modulegraph-boundary* DMG)])
    ;; -- 4 = 1 to get the world, 3 for each command in `zombie-hist-micro.rktd`
    (check-equal?
      (dyn-boundary*-ref/fail B* "main" "zombie")
      '((w0 . 1) (world-on-mouse . 0) (world-on-tick . 3)))
    (check-equal?
      (dyn-boundary*-ref/fail B* "zombie" "image")
      '((circle . 1) (empty-scene . 1) (image . 0) (image-impl . 0) (image? . 0) (place-image . 0) (struct:image . 0)))
    (check-equal?
      (dyn-boundary*-ref/fail B* "zombie" "math")
      '((abs . 90) (max . 18) (min . 9) (msqrt . 9) (sqr . 36))))

  #;(let* ([DMG (check-dmg "mini-tetris")]
         [B* (dynamic-modulegraph-boundary* DMG)])
    ;; -- elim.rkt is unused
    (check-equal?
      (to->total-count DMG "elim")
      0)
    ;; -- main does not use bset
    (check-equal?
      (for/sum ([id+c (in-list (dyn-boundary*-ref/fail B* "main" "bset"))])
        (cdr id+c))
      0)
    ;; -- aux only uses 1 identifier
    (check-equal?
      (dyn-boundary*-ref/fail B* "aux" "tetras")
      '((build-tetra-blocks . 7) (tetra-change-color . 0) (tetra-move . 0) (tetra-overlaps-blocks? . 0) (tetra-rotate-ccw . 0) (tetra-rotate-cw . 0)))
   )

  #;(let* ([DMG (check-dmg "mini-mbta")]
         [B* (dynamic-modulegraph-boundary* DMG)])
    (check-equal?
      (dyn-boundary*-ref/fail B* "run-t" "t-view")
      '((manage% . 1)))
    (check-equal?
      (dyn-boundary*-ref/fail B* "t-view" "t-graph")
      '((read-t-graph . 1)))
    (check-equal?
      (to->total-count DMG "main")
      9))

  (let* ([DMG (check-dmg "mini-quadMB")]
         [B* (dynamic-modulegraph-boundary* DMG)])
    (check-equal?
      (to->total-count DMG "hyphenate")
      0)
    (check-equal?
      (to->total-count DMG "main")
      5)
    (check-equal?
      (to->total-count DMG "quad-main")
      381)
    (check-equal?
      (dyn-boundary*-ref/fail B* "ocm" "ocm-struct")
      '(($ocm . 1) ($ocm-base . 1) ($ocm-entry->value . 1) ($ocm-finished . 5) ($ocm-matrix-proc . 1) ($ocm-min-entrys . 3) ($ocm-min-row-indices . 2) ($ocm-tentative . 2) ($ocm? . 0) (set-$ocm-base! . 0) (set-$ocm-entry->value! . 0) (set-$ocm-finished! . 1) (set-$ocm-matrix-proc! . 0) (set-$ocm-min-entrys! . 1) (set-$ocm-min-row-indices! . 1) (set-$ocm-tentative! . 1) (struct:$ocm . 0))))
)
