#lang racket/base

;; TODO
;; - typed racket 
;; - unit tests

(provide
)

(require
  gtp-summarize/modulegraph
  glob
  (only-in racket/string string-replace)
  (only-in racket/file delete-directory/files)
  (only-in racket/port copy-port)
  (only-in racket/system system)
)

;; =============================================================================

(define TRACE-PREFIX 'trace:)
;; Prefix for indirected identifiers

(define TRACE-TMP ".trace.tmp")
;; Temporary file to use for search/replace

(define *TEX-OUT* (make-parameter "graph.tex"))
;(define *STD-OUT* (make-parameter #f))

(define *VERBOSE* (make-parameter #t))
(define-syntax-rule (debug str arg* ...)
  (when (*VERBOSE*)
    (display "[DEBUG] : ")
    (printf str arg* ...)
    (newline)))

;; =============================================================================

;; (define-type DynamicBoundary (List String String (Listof (Pairof Provided Natural))))
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

;; -----------------------------------------------------------------------------

(define (to->dyn-boundary* dmg to)
  (for/list ([b (in-list (dynamic-modulegraph-boundary* dmg))]
             #:when (string=? to (dyn-boundary-to b)))
    b))

;; Sum the total uses of all identifiers in this boundary
(define (dyn-boundary-count b)
  (for/sum ([p+c (in-list (dyn-boundary-provided+count* b))])
    (cdr p+c)))

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
          (define msg (format "[TRACE]\t~a\t~a\t~a" to from id))
          (printf "(define-syntax (~a stx)\n" id)
          (printf "  (syntax-parse stx\n")
          (printf "   [(_ arg* ...)\n")
          (printf "    (syntax/loc stx (begin\n")
          (printf "      (printf \"~a\\t~~a\\n\" (list arg* ...))\n" msg)
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
  (copy-all (string-append trace-str "/../both") trace-str #:glob "*.rkt"))

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

;; (: directory->dmg (-> Path-String Dynamic-ModuleGraph))
(define (directory->dmg dir)
  (define MG (from-directory dir))
  (define B* (boundaries MG))
  (define trace-dir (setup-trace-dir/check dir B*))

  ;; TODO

  (raise-user-error 'nope))

(define (project-name->dmg name)
  (directory->dmg (infer-project-dir name)))

;; Pretty-print a dynamic module graph to the current output port
;; (: print-dmg (-> DMG Void))
(define (print-dmg dmg)
  ;; Print title
  (define title (format "ModuleGraph for ~a:" (dmg->project-name dmg)))
  (displayln title)
  (displayln (make-string (string-length title) "="))
  (newline)
  ;; For each module, print info about its requires
  (for ([to (in-list (dmg->module-names dmg))])
    (printf "Requires for: ~a.rkt\n" to)
    (define indent "  - ")
    (for ([b (in-list (to->dyn-boundary* dmg to))])
      (printf "  - ~a : ~a\n" (dyn-boundary-from b) (dyn-boundary-count b)))
    (newline)))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "gtp-trace"
   #:once-each
   [("--tex") tex "File to store .tex output to" (*TEX-OUT* tex)]
   [("-q" "--quiet") "Run quietly" (*VERBOSE* #f)]
   #:args (PROJECT-NAME)
   (define DMG (project-name->dmg PROJECT-NAME))
   (print-dmg DMG))
)

;; =============================================================================

(module+ test
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
)
