#lang racket

(provide
 ;; String 
 EOM
 DONE
 
 ;; constants, regexps that match PATH, DISABLE, and ENABLE requests
 PATH 
 DISABLE
 ENABLE
 
 ;; InputPort OutputPort -> Void 
 ;; read FROM, DISABLE, and ENABLE requests input-port, write responses to output-port, loop
 run-t)

;; ===================================================================================================

(require "t-view.rkt")

(define PATH    #rx"from (.*) to (.*)$")
(define DISABLE #rx"disable (.*)$")
(define ENABLE  #rx"enable (.*)$")

(define DONE    "done")
(define EOM     "eom")

(define manage (new manage%))

(define (run-t [input-port (current-input-port)] [output-port (current-output-port)])
  (thread 
   (lambda ()
     (parameterize ([current-input-port input-port]
                    [current-output-port output-port])
       (define next (read-line))
       (unless (eof-object? next)
         (cond
           [(regexp-match PATH next)
            => (lambda (x) (displayln (send manage find (second x) (third x))))]
           [(regexp-match DISABLE next)
            => (lambda (x) (status-check add-to-disabled (second x)))]
           [(regexp-match ENABLE next)
            => (lambda (x) (status-check remove-from-disabled (second x)))]
           [else (displayln "message not understood")])
         (displayln EOM)
         (flush-output))))))

(define-syntax-rule
  (status-check remove-from-disabled enabled)
  (let ([status (send manage remove-from-disabled enabled)])
    (if (boolean? status) 
        (displayln DONE)
        (displayln status))))

