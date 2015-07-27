#lang racket/base

(provide (struct-out anystruct) s1)

(struct anystruct (var fun vec))

(define s1 (anystruct "yolo"
                      (lambda (x y z) (cons z (+ x x y)))
                      (vector 'a 2 3)))
