#lang typed/racket

;; stress testing run-t on 100 look ups, plus 5 [dis|en]ables

;; ===================================================================================================
; (require/typed "run-t.rkt" [EOM String] [run-t (-> Port Port Void)])
(require benchmark-util)
(require/typed "run-t.rkt"
               [EOM String]
               [run-t (-> Input-Port Output-Port Thread)])
                     

(: stress-test (-> Natural Void))
;; run the stress test n times
(define (stress-test n)
  (for ((_i (in-range n)))
    (define-values (in out) (make-pipe))
    (define-values (_in _out) (make-pipe))
    (define c (make-custodian))
    (parameterize ([current-custodian c])
      (start-server in _out)
      (parameterize ([current-input-port _in]
                     [current-output-port out])
        (assert (path "Airport" "Northeastern" 30))
        (assert (able "dis" "Government"))
        (assert (path "Airport" "Northeastern" 10))
        (assert (able "en" "Government"))
        (assert (path "Airport" "Harvard Square" 20))
        (assert (able "dis" "Park"))
        (assert (path "Northeastern" "Harvard Square" 20))
        (assert (able "en" "Park"))
        (assert (path "Northeastern" "Harvard Square" 20))))
    (custodian-shutdown-all c)))

(: start-server (-> Input-Port Output-Port Thread))
(define (start-server in _out)
  (thread
   (lambda ()
     (let loop ()
       (run-t in _out)
       (loop)))))

(: path (-> String String Natural [Listof [Listof String]]))
(define (path from to n) 
  (for/list : [Listof [Listof String]] ([_ (in-range n)]) 
            (printf "from ~a to ~a\n" from to)
            (read-to EOM)))

;; Symbol String -> Void
(define (able dis-en s) 
  (printf "~aable ~a\n" dis-en s)
  (read-to EOM))

(define-syntax assert
  (syntax-rules ()
    [(_ (f x y n)) (unless (= (length (f x y n)) n) (error "assert"))]
    [(_ (f x y)) 
     (let ([r (f x y)])
       (unless (= (length r) 1) (error "assert2")))]))

(: read-to (-> String [Listof String]))
;; read up to x and collect lines into list
(define (read-to x)
  (define next (read-line))
  (if (or (eof-object? next) (string=? x (string-trim next)))
      '()
      (cons next (read-to x))))

(time (stress-test 1))
