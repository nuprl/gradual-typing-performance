#lang racket

(require 2htdp/image
         "data.rkt")

(provide   
 overlay
 circle
 rectangle
 place-image
 empty-scene)
#;
(provide/contract
 [overlay (image/c image/c . -> . image/c)]
 [circle (integer? string? string? . -> . image/c)]
 [rectangle (integer? integer? COLOR/C COLOR/C . -> . image/c)]
 [place-image (image/c integer? integer? image/c . -> . image/c)]
 [empty-scene (integer? integer? . -> . image/c)])

