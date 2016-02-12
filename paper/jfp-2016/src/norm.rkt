#lang racket/base

(module norm typed/racket

  (define-type Point%
    (Class (field [x Real] [y Real])))
  (define-type Point (Instance Point%))

  (: norm (-> Point Real))
  (define (norm p)
    (+ (abs (get-field x p))
       (abs (get-field y p))))

  (provide Point norm))

(module user racket
  (require (submod ".." norm))

  (norm 3))

(require 'user)
