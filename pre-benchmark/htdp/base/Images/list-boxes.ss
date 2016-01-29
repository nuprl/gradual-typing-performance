#lang slideshow

(require "box-diagram.ss" "write.ss")

(define (b) (cc-superimpose (disk 5) (text " ") (blank 30 10)))
(define mbullet (b))
(define vbullet (b))

(define mercury
  (struct-pict "cons" `(("first" "\"Mercury\"") ("rest" "empty"))))
(define venus
  (struct-pict "cons" `(("first" "\"Venus\"") ("rest" ,mbullet))))
(define earth
  (struct-pict "cons" `(("first" "\"Earth\"") ("rest" ,vbullet))))

(define delta 50)
(define (all mercury venus earth)
  (let* ([blk (blank 10 1)]
         [all (hb-append mercury blk venus blk earth)]
         [wdt (pict-width all)]
         [hgt (pict-height all)])
    (cc-superimpose (blank (+ wdt delta) (+ hgt delta)) all)))

(define right-to-left (all mercury venus earth))
(define left-to-right (all earth venus mercury))

(define (connect base from to rb-find delta)
  (define (f base p)
    (define-values (x y) (rb-find base p))
    (values (+ x delta) y))
  (pin-arrow-line 5 base
                  from cc-find
                  to f ; rb-find
                  #:start-angle (* -2/4 pi)
                  #:start-pull 1/2
                  #:end-angle (* 2/4 pi)))

(write-pict "list-boxes" 
            (let* ([one (connect right-to-left mbullet mercury rb-find -10)])
              (connect one vbullet venus rb-find -10)))

(write-pict "list-boxes2" 
            (let* ([one (connect left-to-right mbullet mercury lb-find +10)])
              (connect one vbullet venus lb-find +10)))
