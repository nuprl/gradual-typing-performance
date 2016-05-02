#lang typed/racket

;; stress testing run-t on 100 look ups, plus 5 [dis|en]ables

;; ===================================================================================================
(require require-typed-check)
(require (only-in racket/string string-join))
(require/typed/check "run-t.rkt"
               [EOM String]
               [run-t (-> String String)])

(: dat->station-names (-> Path-String (Listof String)))
(define (dat->station-names fname)
  (for/list ([line (in-list (file->lines fname))]
             #:when (and (< 0 (string-length line))
                         (not (eq? #\- (string-ref line 0)))))
    (string-trim line)))

(define BLUE-STATIONS (dat->station-names "../base/blue.dat"))
(define GREEN-STATIONS (dat->station-names "../base/green.dat"))

(: path (-> String String String))
(define (path from to)
  (format "from ~a to ~a" from to))

(: enable (-> String String))
(define (enable s)
  (format "enable ~a" s))

(: disable (-> String String))
(define (disable s)
  (format "disable ~a" s))

(: assert (-> String Natural Void))
(define (assert result expected-length)
  (define num-result (length (string-split result "\n")))
  (unless (= num-result expected-length)
    (error (format "Expected ~a results, got ~a\nFull list:~a"
                   expected-length
                   num-result
                   result))))

(: main (-> Void))
;; run the stress test n times
(define (main)
  (: run-query (-> String String))
  (define (run-query str)
    (define r (run-t str))
    (if r
        r
        (error 'main (format "run-t failed to respond to query ~e\n" str))))
  (assert (run-query (path "Airport" "Northeastern")) 14)
  (assert (run-query (disable "Government")) 1)
  (assert (run-query (path "Airport" "Northeastern")) 16)
  (assert (run-query (enable "Government")) 1)
  (assert (run-query (path "Airport" "Harvard Square")) 12)
  (assert (run-query (disable "Park Street")) 1)
  (assert (run-query (path "Northeastern" "Harvard Square")) 1) ;;impossible path
  (assert (run-query (enable "Park Street")) 1)
  (assert (run-query (path "Northeastern" "Harvard Square")) 12)
  ;; --
  (for* ([s1 (in-list GREEN-STATIONS)] [s2 (in-list BLUE-STATIONS)])
    (run-query (path s1 s2))))

;(require/typed contract-profile [contract-profile-thunk (-> (-> Void) Void)])
;(contract-profile-thunk main)
(time (main))
