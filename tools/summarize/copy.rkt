#lang racket/base

;; Usage:
;;    raco gtp-copy <benchmark-dir> <bitstring>
;; Given the name of a benchmark with N files and an N-bit number
;;  (represented as a string of 1 and 0)
;;  copy the corresponding typed and untyped files from the benchmark
;;  to the current directory.

(require
  file/glob
  racket/file
  racket/path)

;; =============================================================================

(define BASE "base")
(define BOTH "both")
(define TYPED "typed")
(define UNTYPED "untyped")

(define (benchmark-directory? x)
  (and (path-string? x)
       (directory-exists? x)
       (directory-exists? (build-path x TYPED))
       (directory-exists? (build-path x UNTYPED))))

(define (bitstring? x)
  (and (string? x)
       (for/and ([c (in-string x)])
         (or (eq? c #\1)
             (eq? c #\0)))))

(define (copy-base-dir bm-dir dst)
  (define base (build-path bm-dir BASE))
  (when (directory-exists? base)
    (copy-directory/files base (build-path dst BASE))))

(define (copy-both-dir bm-dir dst)
  (define both (build-path bm-dir BOTH))
  (when (directory-exists? both)
    (for ((fn (in-glob (build-path both "*"))))
      (copy-directory/files fn (build-path dst (file-name-from-path fn))))))

(define (copy-files/bitstring bm-dir bits dst)
  (define u (build-path bm-dir UNTYPED))
  (define t (build-path bm-dir TYPED))
  (define filename*
    (let ([u* (glob (build-path u "*.rkt"))]
          [t* (glob (build-path t "*.rkt"))])
      (define L (length u*))
      (define B (string-length bits))
      (unless (= L (length t*))
        (raise-user-error 'gtp-copy "refusing to copy files in ~a, typed and untyped directories have a different contents" bm-dir))
      (unless (= L B)
        (raise-user-error 'gtp-copy "refusing to copy files in ~a, have ~a source files and ~a-bit configuration (~a)" bm-dir L B))
      (map file-name-from-path t*)))
  (for ([b (in-string bits)]
        [fn (in-list filename*)])
    (copy-file (build-path (case b [(#\0) u] [(#\1) t]) fn) (build-path dst fn)))
  (void))

(define (gtp-copy bm-dir bits dst)
  (unless (benchmark-directory? bm-dir)
    (raise-argument-error 'gtp-copy "benchmark-directory?" 0 bm-dir bits dst))
  (unless (bitstring? bits)
    (raise-argument-error 'gtp-copy "bitstring?" 1 bm-dir bits dst))
  (unless (path-string? dst)
    (raise-argument-error 'gtp-copy "path-string?" 2 bm-dir bits dst))
  (when (file-exists? dst)
    (raise-user-error 'gtp-copy "destination '~a' is a file, move it or select a new output directory" dst))
  (unless (directory-exists? dst)
    (make-directory dst))
  (copy-files/bitstring bm-dir bits dst)
  (copy-base-dir bm-dir dst)
  (copy-both-dir bm-dir dst)
  (void))


;; =============================================================================

(module+ main
  (require racket/cmdline)
  (define *output-dir* (make-parameter (current-directory)))
  (command-line
    #:program "gtp-copy"
    #:once-each
    [("-o" "--output") od "Output directory" (*output-dir* od)]
    #:args (benchmark-dir bitstring)
    (gtp-copy benchmark-dir bitstring (*output-dir*))))
