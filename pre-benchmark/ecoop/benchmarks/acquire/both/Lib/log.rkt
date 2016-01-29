#lang racket

(provide
 (contract-out 
  (log (-> (list/c any/c any/c) any/c any))))

;; [List X (U String Exn S-expr)] String -> Void 
(define (log status msg)
  (define (racket-loging)
    (log-info msg)
    (define xn (second status))
    (cond
      [(string? xn) (log-info xn)]
      [(exn? xn) (with-output-to-string (lambda () ((error-display-handler) (exn-message xn) xn)))]
      [else (with-output-to-string (lambda () (pretty-print xn)))]))
  (define (my-logging)
    (displayln msg)
    (define xn (second status))
    (cond
      [(string? xn) (displayln xn)]
      [(exn? xn) ((error-display-handler) (exn-message xn) xn)]
      [else (pretty-print xn)]))
  #;
  (racket-logging)
  (my-logging))
