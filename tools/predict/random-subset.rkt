#lang racket/base

;; Start with a full dataset,
;;  randomly pick some configurations
;;  compare the full overhead plot against one using the small data.

;; 2016-08-17: Looks good!
;;  On all the 8+ module benchmarks,
;;   the LNM plot using 1/8th of configs (randomly selected)
;;   is very similar to the full results' LNM plot.

;; -----------------------------------------------------------------------------

(provide
)

(require
  gtp-summarize/summary
  gtp-summarize/modulegraph
  gtp-summarize/render-lnm

  glob

  (only-in racket/file
    file->value)
  racket/string
  racket/path
  (only-in racket/list
    last)
  (only-in gtp-summarize/bitstring
    log2)
  (only-in racket/random
    random-sample)
  racket/set
)

;; =============================================================================

(define *RKTD-DIR* (make-parameter "./rktd"))

;; -----------------------------------------------------------------------------

(define (sub-rktd rktd)
  (define name (path->project-name rktd))
  (define idx (get-prediction-number rktd))
  (define tag (last (string-split rktd "-")))
  (define parent (rktd->predict-parent rktd))
  (define new-rktd (build-path parent (path-replace-extension (string-append "p" (path->string (file-name-from-path rktd))) ".mini.rktd")))
  (define old-vec (file->value rktd))
  (define num-configs (vector-length old-vec))
  (define num-get (expt 2 (- (log2 num-configs) 3)))
  (define sample (random-sample (in-range num-configs) num-get #:replacement? #f))
  (define new-vec
    (for/vector #:length num-get
                ((i (in-list sample)))
      (vector-ref old-vec i)))
  (vector-set! new-vec 0 (vector-ref old-vec 0))
  (with-output-to-file new-rktd #:exists 'replace
    (lambda ()
      (display ";; ") (writeln sample)
      (displayln "#(")
      (for ((x (in-vector new-vec)))
        (writeln x))
      (displayln ")")))
  (path->string new-rktd))

(define (predict-for rktd [pre-output #f])
  (define output (or pre-output
                     (build-path (rktd->predict-parent rktd)
                                 (path-replace-extension (file-name-from-path rktd) ".png"))))
  (define rktd+ (sub-rktd rktd))
  (define pict (render-lnm (vector "-g" "#f" rktd rktd+)))
  (printf "Saving pict at '~a' (data at '~a')\n" output rktd+)
  (pict->png pict output)
  output)

;; -----------------------------------------------------------------------------

(define (rktd->predict-parent rktd)
  (define vers (string->version rktd))
  (unless (directory-exists? vers)
    (make-directory vers))
  (define name (path->project-name rktd))
  (define parent (build-path vers name))
  (unless (directory-exists? parent)
    (make-directory parent))
  parent)

;; Should parse `*RKTD-DIR*`,
;;  find files matching `rktd`,
;;  return a new "index" bigger than any others
(define (get-prediction-number rktd)
  0)

(define (glob-first str)
  (define m (glob str))
  (if (null? m)
    #f
    (car m)))

;; =============================================================================

(define V '("6.2" "6.3" "6.4" "6.5"))
(define B '("snake" "acquire" "tetris" "synth" "gregor" "quadBG"))

(define (go)
  (define root "/Users/ben/code/racket/gtp/gradual-typing-performance")
  (for* ((v (in-list V))
         (bm (in-list B)))
      (define rktd (glob-first (format "~a/data/~a/~a-*" root v bm)))
      (when (and rktd (not (string-contains? rktd "yolo")))
        (define png (predict-for rktd))
        (copy-file png "output.png" #t)
        (display "ready? ")
        (read-line))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:args ()
    (go)))

