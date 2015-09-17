#lang racket/base

;; Parser for FSP programs

;; Very specific right now
;; - accept a bunch of folder names
;; - for each folder, average the runtimes
;; (AKA, the 'specific feature-specific-profiler parser')

(require
 glob
 math/statistics
 (only-in racket/list last)
 (only-in racket/string string-split)
 (only-in racket/format ~r)
 )

;; =============================================================================

;; Regexp to get the "CONTRACT-TIME / TOTAL-TIME ms" line from FSP output.
;; The numbers could be fractions (i.e., 1/3), hence the 'dumb' regexp
(define rCTIME (regexp "^(.+) / (.+) ms$"))
;; (it's a face)

;; (-> Path-String (-> Number Number Number) (U Number #f))
(define (parse-contract/total fname f)
  (with-input-from-file fname
    (lambda ()
      (for/or ([ln (in-lines)])
        (let ([m (regexp-match rCTIME ln)])
          (and m (f (string->number (cadr m))
                    (string->number (caddr m)))))))))

(define (parse-contract-proportion fname)
  (parse-contract/total fname (lambda (a b) (* 100 (/ a b)))))

(define (parse-total-runtime fname)
  (parse-contract/total fname (lambda (a b) b)))

(define (r n)
  (~r n #:precision '(= 2)))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "fsp-parse"
   #:args DIR*
   (for ([d (in-list DIR*)]
         #:when (or (directory-exists? d)
                    (begin
                      (printf "ERROR: cannot process non-directory '~a'\n" d)
                      #f)))
     (define project-name (last (string-split d "/")))
     (define ctime*
       (for/list ([f (in-glob (format "~a/*.txt" d))])
         (or (parse-total-runtime f)
             (raise-user-error 'parse-fsp (format "Could not read file '~e'" f)))))
     (printf "~a : ~a (~a)\n" project-name (r (mean ctime*)) (r (stddev ctime*))))))
