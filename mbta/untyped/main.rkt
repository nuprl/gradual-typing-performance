#lang racket

;; stress testing run-t on 100 look ups, plus 5 [dis|en]ables

;; ===================================================================================================
(require "run-t.rkt"
         (only-in racket/string string-join))

;; Nat -> Void 
;; run the stress test n times
(define (stress-test n)
  (for ((_i (in-range n)))
    (define-values (in out) (make-pipe))
    (define-values (_in _out) (make-pipe))
    (define c (make-custodian))
    (parameterize ([current-custodian c])
      (define (run-query str)
        (run-t in _out)
        (displayln str)
        (read-to EOM))
      (parameterize ([current-input-port _in]
                     [current-output-port out])
        (assert (run-query (path "Airport" "Northeastern")) 14)
        (assert (run-query (disable "Government")) 1)
        (assert (run-query (path "Airport" "Northeastern")) 16)
        (assert (run-query (enable "Government")) 1)
        (assert (run-query (path "Airport" "Harvard Square")) 12)
        (assert (run-query (disable "Park Street")) 1)
        (assert (run-query (path "Northeastern" "Harvard Square")) 1) ;;impossible path
        (assert (run-query (enable "Park Street")) 1)
        (assert (run-query (path "Northeastern" "Harvard Square")) 12)))
    (custodian-shutdown-all c)))

;; String String -> String
(define (path from to)
  (format "from ~a to ~a" from to))

;; String -> String
(define (enable s)
  (format "enable ~a" s))

(define (disable s)
  (format "disable ~a" s))

(define (assert result-list expected-length)
  (define SEP "\n    ")
  (unless (= (length result-list) expected-length)
    (error (format "Expected ~a results, got ~a\nFull list:~a~a"
                   expected-length
                   (length result-list)
                   SEP
                   (string-join result-list SEP)))))

;; -> [Listof String]
;; read up to x and collect lines into list
(define (read-to x)
  (define next (read-line))
  (if (or (eof-object? next) (string=? x (string-trim next)))
      '()
      (cons next (read-to x))))

(time (stress-test 10))
