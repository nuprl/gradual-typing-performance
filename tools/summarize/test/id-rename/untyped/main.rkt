#lang racket/base

(require "friend.rkt")

(define y x)
(begin x y x y (void))
