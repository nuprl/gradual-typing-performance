#lang racket

;; Problems:
;; - no arrows
;; - no diamonds
;; - 

(require pict/tree-layout)

;; Should be a diamond, but I guess tree-layouts don't allow that
(define echo
  (let ([leaf (tree-layout)])
    (naive-layered (tree-layout (tree-layout leaf) (tree-layout leaf)))))

(define sieve
  (naive-layered (tree-layout (tree-layout))))