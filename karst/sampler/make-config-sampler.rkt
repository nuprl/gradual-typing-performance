#lang racket/base

;; Make a .tar.gz file of sample configurations to try running.
;; Goal: running these takes less than 24 hours,
;;        should prolly aim for 16-20 hours

;; Maybe should generate the scripts too,
;;  could share the env variables that way
;; (Yo, could just lookup how to do that in bash)

;; -----------------------------------------------------------------------------

(require racket/file "../../../tools/setup-benchmark.rkt")


;; =============================================================================

(define GT (simplify-path (build-path (current-directory) 'up 'up 'up)))

(define OUT-DIR (build-path (current-directory) "config-sampler"))
(define OUT-TAR (path-replace-extension OUT-DIR ".tar.gz"))

;; Hm, could auto-generate and compute things that will run in time

(define interesting-configs '(
  ("sieve" "00" "11")
  ("forth" "1100" "0001")
  ("fsm" "0110" "1000")
  ("fsmoo" "0101" "0000")
  ("mbta" "1110" "1101")
  ("morsecode" "0011" "1010")
  ("zombie" "0000" "0101")
  ("dungeon" "01010" "11101")
  ;("zordoz.6.5" "01011" "10101")
  ("lnm" "001111" "111010")
  ("suffixtree" "001000" "111111")
  ("kcfa" "0001100" "1111111")
  ("snake" "11111011" "00110111")
  ("take5" "01110101" "11011110")
  ("acquire" "110101001" "010100010")
  ("tetris" "011100001" "111111011")
  ("synth" "0111110111" "1100100011")
  ("gregor" "0111111000111" "0000111001111")
  ("quadBG" "11100111111101" "01110010110101")
))

(module+ main
  ;; --
  (when (directory-exists? OUT-DIR)
    (raise-user-error 'make-config-sampler "Output folder '~a' already exists. Delete and try again." OUT-DIR))
  (make-directory OUT-DIR)
  ;; --
  (for ((yo (in-list interesting-configs)))
    (define bm (car yo))
    (define cfg* (cdr yo))
    (define bm-OUT-DIR (build-path OUT-DIR bm))
    (define bm-GT (build-path GT "benchmarks" bm "benchmark"))
    (make-directory bm-OUT-DIR)
    (parameterize ([current-directory GT])
      (create-benchmark-dirs (format "benchmarks/~a" bm)))
    (let ([base (build-path bm-GT "base")])
      (when (directory-exists? base)
        (copy-directory/files base
                              (build-path bm-OUT-DIR "base"))))
    (for ((cfg (in-list cfg*)))
      (copy-directory/files (build-path bm-GT (format "configuration~a" cfg))
                            (build-path bm-OUT-DIR cfg)))
    (delete-directory/files bm-GT)
    (void))
)

