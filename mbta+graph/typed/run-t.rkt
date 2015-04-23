#lang typed/racket

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
(require benchmark-util)
(require/typed/check "t-view.rkt" [manage% Manage])
(require "graph-types.rkt")

(define PATH    #rx"from (.*) to (.*)$")
(define DISABLE #rx"disable (.*)$")
(define ENABLE  #rx"enable (.*)$")

(define DONE    "done")
(define EOM     "eom")

(: manage [Instance Manage])
(define manage (new manage%))

(: run-t (-> Input-Port Output-Port Thread))
(define (run-t [input-port (current-input-port)] [output-port (current-output-port)])
  (thread 
   (lambda ()
     (parameterize ([current-input-port input-port]
                    [current-output-port output-port])
       (define next (read-line))
       (unless (eof-object? next)
         (cond
           [(regexp-match PATH next)
            => (lambda (x)
                 (define y (cast x [List String String String]))
                 (displayln (send manage find (second y) (third y))))]
           [(regexp-match DISABLE next)
            => (lambda (x) 
                 (define y (cast x [List String String]))
                 (status-check add-to-disabled (second y)))]
           [(regexp-match ENABLE next)
            => (lambda (x)
                 (define y (cast x [List String String]))
                 (status-check remove-from-disabled (second y)))]
           [else (displayln "message not understood")])
         (displayln EOM)
         (flush-output))))))

(define-syntax-rule
  (status-check remove-from-disabled enabled)
  (let ([status (send manage remove-from-disabled enabled)])
    (if (boolean? status) 
        (displayln DONE)
        (displayln status))))

