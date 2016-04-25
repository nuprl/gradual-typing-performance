#lang racket
(require pfds/trie)

(define t (trie (list (range 128))))
(define u (time (bind (range 128) 0 t)))
