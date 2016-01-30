#lang racket

;; I/O routines for reading XML from ports and writing it to ports
(provide
 ;; utilities:
 ;; Symbol X -> X 
 ;; displayln tagged X then return it 
 tee
  
 ;; tee with pretty-print 
 tee/p

 ;; [InputPort] -> Xexpr | EOF 
 ;; read XML element from InputPort and convert Xexpr; or signal EOF 
 read-xml-from
 
 ;; Xexpr [OutputPort] -> Void
 ;; write Xexpr to OutputPort and flush
 write-xml-to
 
 ;; (tester directory:string n:natural function:expr argument:expr)
 ;; run (function arg)
 ;; effect: put input into directory and in<n>.xml
 ;; effect: put output into directory and out<n>.xml
 ;; depends on parameter create-tests
 ;; #f -> do not create files 
 ;; #t -> do create files 
 tester
 
 ;; parameter
 create-tests)

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

(require xml)

(define read-xml-from
  (case-lambda 
    [() (read-xml-from (current-input-port))]
    [(ip)
     (with-handlers ((exn:xml? (λ (x) eof)))
       (xml->xexpr ((eliminate-whitespace '() (λ (x) #t)) (read-xml/element ip))))]))

(define write-xml-to
  (case-lambda
    [(x op)
     (display-xml/content (xexpr->xml x) op)
     ; (write-xexpr x op)
     (newline op)
     (flush-output op)]
    [(x)
     (write-xml-to x (current-output-port))]))


(define-syntax-rule 
  (tester directory n function arg)
  (if (create-tests)
      (let ([in (format "~a/in~a.xml" directory n)]
            [out (format "~a/out~a.xml" directory n)])
        (testing out (function (testing in arg))))
      (function arg)))

(define create-tests (make-parameter #f))

;; String Xexpr -> Void
(define (testing name x)
  ;; add x as XML to name
  (define target name)
  (with-output-to-file target
    #:exists 'append
    (lambda ()
      (write-xml-to x)))
  x)

(define (tee t x)
  (displayln `(,t ,x))
  x)

(define (tee/p t x)
  (pretty-print `(,t ,x))
  x)
