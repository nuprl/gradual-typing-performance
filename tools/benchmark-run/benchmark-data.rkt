#lang racket/base

(provide
  assert-benchmark?
  benchmark-paths

  BASE
  BOTH
  TYPED
  UNTYPED
  BENCHMARK
  CONFIGURATION
)

(require
  glob
  benchmark-run/utilities
  (only-in racket/string string-suffix?)
)

;; =============================================================================

(define BASE "base")
(define BOTH "both")
(define TYPED "typed")
(define UNTYPED "untyped")
(define COMMENT (string-append ";; " (make-string 77 #\-)))
(define BENCHMARK "benchmark")
(define CONFIGURATION "configuration")

(define ERRLOC 'setup-benchmark)

(define (benchmark-paths pwd)
  (define base (build-path pwd "base"))
  (define both (build-path pwd "both"))
  (define typed (build-path pwd TYPED))
  (define untyped (build-path pwd UNTYPED))
  (define bdir (build-path pwd BENCHMARK))
  (values base both typed untyped bdir))

(define (assert-benchmark? pwd)
  (define-values (base both typed untyped bdir) (benchmark-paths pwd))
  ;; (1) make sure directories base, typed, and untyped exist 
  ;; (2) the typed and untyped contain the same number of files 
  ;; (3) ... and files with the same names
  ;; Note: 'both' directory is optional
  (define typed-dir (filter rkt-file? (directory-list typed)))
  (define untyped-dir (filter rkt-file? (directory-list untyped)))
  (and (files-exist? pwd typed untyped)
       (same-number-of-files? typed-dir untyped-dir)
       (same-file-names? typed-dir untyped-dir)
       #t))

;; [Listof Path] [Listof Path] -> Void
(define (same-file-names? typed-dir untyped-dir)
  (unless (for/and ([tp (in-list typed-dir)]
                    [up (in-list untyped-dir)])
            (string=? (path->string tp) (path->string up)))
    (raise-user-error ERRLOC
      "Typed and Untyped directories do not contain the same file names.")))

;; String *-> Boolean 
(define (files-exist? . dir*)
  (for ([dir (in-list dir*)])
    (unless (directory-exists? dir)
      (raise-user-error ERRLOC "directory `~a' does not exist" dir))))

;; [Listof X] [Listof Y] -> Void
(define (same-number-of-files? typed-dir untyped-dir)
  (unless (= (length typed-dir) (length untyped-dir))
    (raise-user-error ERRLOC
      "Typed and Untyped directories do not have the same number of files")))

;; =============================================================================

(module+ test
  (require rackunit)

  (let*-values (((FOO) "foo")
                ((base both typed untyped bdir) (benchmark-paths "foo")))
    (check-equal? base (build-path FOO BASE))
    (check-equal? both (build-path FOO BOTH))
    (check-equal? typed (build-path FOO TYPED))
    (check-equal? untyped (build-path FOO UNTYPED))
    (check-equal? bdir (build-path FOO BENCHMARK)))

  (check-true (assert-benchmark? "../../benchmarks/test-success"))

)
