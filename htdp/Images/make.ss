#lang scheme

(define ss
  (filter (lambda (f)
            (define p (path->string f))
            (and (not (string=? "car.ss" p)) (regexp-match ".ss$" p)))
          (directory-list)))
(define cd (current-directory))

(for-each (lambda (s) (dynamic-require (build-path cd s) #f)) ss)