#lang racket

;; ---------------------------------------------------------------------------------------------------
;; a generic script generator
;; generated script runs client on specified in<n>.xml files
;; and compares the expected XML output with outN.xml
;; via some xml-diff checker 

(provide main test-directory xml-diff xclient)

;; systems require: 
;; it relies on a script in xdiff-xml that compares XML outputs in the
;; context of SwDev projects
(define test-directory (make-parameter #f))
(define xml-diff       (make-parameter #f))
(define xclient        (make-parameter #f))

;; ---------------------------------------------------------------------------------------------------
;; implementation 

(define (main . x)
  (define files
    (parameterize ((current-directory (test-directory)))
      (for/list ((f (directory-list)) #:when (regexp-match #px"in(\\d).xml$" (path->string f)))
	(second (regexp-match #px"in(\\d).xml$" (path->string f))))))
  (for ((n files))
    (test n)))

;; String [Nat] -> Boolean 
(define (test n)
  (displayln "-----------------------------------------------------------------------------")
  (printf    "testing in~a.xml, out~a.xml\n" n n)
  (define client (format (xclient) (test-directory) n))
  (define xdiff  (format (xml-diff) (test-directory) n))
  (system (format "~a | ~a " client xdiff))
  (void))

