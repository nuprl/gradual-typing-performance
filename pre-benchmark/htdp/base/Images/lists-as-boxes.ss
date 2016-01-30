#lang slideshow

(require "box-diagram.ss")

(define mercury
  (draw-box "mercury" "cons" `(("first" "\"Mercury\"") ("rest" "empty"))))
(define venus
  (draw-box #:color "magenta" "venus" "cons" `(("first" "\"Venus\"") ("rest" ,mercury))))
(define earth
  (draw-box #:color "blue" "earth" "cons" `(("first" "\"Earth\"") ("rest" ,venus))))

