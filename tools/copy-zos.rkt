#lang racket/base

;; This script compiles the fully untyped and fully typed configurations and then assembles
;; bytecodes for the rest of the variations based on the bytecode of those two configurations.
;;
;; This should only work when the modules only have value-dependencies. It won't work if there
;; are macro-dependencies because this may result in inconsistent bytecodes.
;;
;; ASSUMPTIONS: run after setup.rkt
;;              run in the root folder of the GT perf repo

(require racket/match
         racket/system
         compiler/compilation-path
         data/bit-vector)

(define TYPED     "typed")
(define UNTYPED   "untyped")
(define BASE      "base")
(define BOTH      "both")
(define BENCHMARK "benchmark")
(define MAIN      "main.rkt")

(define (assemble program-name)
  (define benchmark-dir (build-path program-name "benchmark"))
  (define all-configs
    (filter (λ (d) (regexp-match #rx"^variation" d))
            (directory-list benchmark-dir)))
  (define full-untyped
    (findf (λ (d) (regexp-match #rx"variation0+$" d)) all-configs))
  (define full-typed
    (findf (λ (d) (regexp-match #rx"variation1+$" d)) all-configs))
  (define mod-paths
    (filter (λ (d) (regexp-match #rx".rkt$" d))
            (directory-list (build-path program-name "typed"))))

  (define (compile-one p)
    (system (format "raco make -v ~a" (build-path benchmark-dir p MAIN))))
  (compile-one full-untyped)
  (compile-one full-typed)

  (move-zos benchmark-dir
            all-configs
            mod-paths
            full-untyped
            full-typed))

(define (move-zos benchmark-dir all-configs mod-paths full-untyped full-typed)
  (for ([config (in-list all-configs)]
        #:unless (equal? config full-untyped)
        #:unless (equal? config full-typed))
    (match-define (list _ bits)
                  (regexp-match #rx"variation(.*)$" config))
    (define bv (string->bit-vector bits))
    (printf "--------------------------------------------~n")
    (printf "movings files for config ~a~n" (path->string config))
    (printf "--------------------------------------------~n")

    (define compiled-dir
      (get-compilation-dir (build-path benchmark-dir config "dummy")))
    (unless (directory-exists? compiled-dir)
      (make-directory compiled-dir))

    (for ([bit (in-bit-vector bv)]
          [mod (in-list mod-paths)])
      (define source
        (build-path benchmark-dir
                    (if bit full-typed full-untyped)
                    (get-compilation-bytecode-file mod)))
      (define target
        (build-path benchmark-dir config (get-compilation-bytecode-file mod)))
      (printf "copying ~a to ~a~n" source target)
      (copy-file source target #t))))

(module+ main
  (match (current-command-line-arguments)
    [(vector name) (assemble name)]))
