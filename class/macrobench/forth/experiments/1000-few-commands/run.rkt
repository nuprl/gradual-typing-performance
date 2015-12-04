#lang racket

(for ([i (in-list '(1 5 10 15 20 25 30 35 40 45))])
  (define F (format "history-~a.txt" i))
  (with-output-to-file F #:exists 'replace
    (lambda ()
      (for ([_i (in-range i)])
        (displayln 'swap))))
  (displayln i)
  (system (format "racket6.3 main.rkt ~a" F)))
