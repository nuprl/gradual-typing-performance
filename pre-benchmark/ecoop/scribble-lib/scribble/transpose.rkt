#lang racket

(provide transpose)

(define (transpose lst) (apply map list lst))