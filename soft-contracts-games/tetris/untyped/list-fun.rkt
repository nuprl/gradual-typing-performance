#lang racket

(require "image.rkt"
         "data.rkt")

(define foldr-i foldr)
(define foldr-n foldr)
(provide/contract
 [max (real? real? . -> . real?)]
 [min (real? real? . -> . real?)]
 [ormap ([BLOCK/C . -> . boolean?] (listof any/c) . -> . boolean?)]
 [andmap ([BLOCK/C . -> . boolean?] (listof any/c) . -> . boolean?)]
 [map ([BLOCK/C . -> . BLOCK/C] BSET/C . -> . BSET/C)]
 [filter ([BLOCK/C . -> . boolean?] BSET/C . -> . BSET/C)]
 [append (BSET/C BSET/C . -> . BSET/C)]
 [length ((listof any/c) . -> . integer?)]
 [foldr ([BLOCK/C BSET/C . -> . BSET/C] BSET/C BSET/C . -> . BSET/C)]
 [foldr-i ([BLOCK/C image/c . -> . image/c] image/c BSET/C . -> . image/c)]
 [foldr-n ((BLOCK/C real? . -> . real?) real? BSET/C . -> . real?)])
