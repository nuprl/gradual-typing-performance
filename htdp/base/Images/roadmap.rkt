#lang racket

;; -----------------------------------------------------------------------------
(require "ysupport.rkt" slideshow)

(define (chapter title)
  (cc-superimpose
   (t title)
   (rounded-rectangle width height)))

(define pro "Prologue")
(define fix "Fixed Size Data")
(define arb "Arbitrarily Large Data")
(define lam "Abstraction")
(define sex "Intertwinded Data")
(define gen "Generative Recursion")
(define acc "Accumulators")

(define all (list pro fix arb lam sex gen acc))

(define width  (+ 10 (apply max (map (compose pict-width t) all))))
(define height (+ 4 (apply max (map (compose pict-height t) all))))

(define all-chapters (map chapter all))

(build-scene 
 s (apply vc-append 33 all-chapters)
 (let loop ([s s][chapter0 (first all-chapters)][c (rest all-chapters)])
   (cond
     [(empty? c) s]
     [else
      (define f (first c))
      (pin-arrow-line 10 (loop s f (rest c)) chapter0 cb-find f ct-find)])))