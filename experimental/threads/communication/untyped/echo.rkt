#lang racket/base

(provide echo)

(define RESPONSE (make-string 1000))

(define (echo in out)
  (thread
   (lambda ()
     (parameterize ([current-input-port in]
                    [current-output-port out])
       (define next (read-line))
       (displayln RESPONSE)))))
