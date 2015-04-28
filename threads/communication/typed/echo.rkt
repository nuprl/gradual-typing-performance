#lang typed/racket/base

(provide echo)

(: RESPONSE String)
(define RESPONSE (make-string 1000))

(: echo (-> Input-Port Output-Port Thread))
(define (echo in out)
  (thread
   (lambda ()
     (parameterize ([current-input-port in]
                    [current-output-port out])
       (define next (read-line))
       (displayln RESPONSE)))))
