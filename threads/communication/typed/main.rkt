#lang typed/racket/base

(require benchmark-util)
(require/typed/check 
 "echo.rkt"
 [echo (-> Input-Port Output-Port Thread)])

(: MESSAGE String)
(define MESSAGE (make-string 1000))

(: server-loop (-> Input-Port Output-Port (-> Any)))
(define (server-loop in out)
  (lambda ()
    (echo in out)))

(: main (-> Natural Void))
(define (main n)
  (for ((_i (in-range n)))
    (define-values (in out) (make-pipe))
    (define-values (_in _out) (make-pipe))
    (define c (make-custodian))
    (parameterize ([current-custodian c])
      (define server (thread (server-loop in _out)))
      (parameterize ([current-input-port _in]
                     [current-output-port out])
        (displayln MESSAGE)
        (read-line)))))

(time (main 1000))

