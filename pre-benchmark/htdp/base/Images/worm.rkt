#lang racket

; (require 2htdp/image)

(require (prefix-in fake: "image-fakes.ss") slideshow "write.ss" #;"pict-aux.ss")

(define mt (fake:empty-scene 100 100))

(define dia  3)
(define food (fake:circle dia "solid" "green"))
(define worm (fake:circle dia "solid" "red"))

;; --- worm1 

(write-pict "worm1" (fake:place-image worm 30 55 (fake:place-image food 45 55 mt)))

;; --- worm2 

(define s1 mt)
(define s2 (foldl (lambda (y s) (fake:place-image worm 30 y s)) s1 (build-list 3 (lambda (i) (+ 20 (* i 2 dia))))))
(define s3 (fake:place-image worm (+ 30 dia dia) (+ 20 (* 4 dia)) s2))
(define s4 (foldl (lambda (y s) (fake:place-image worm (+ 30 dia dia) y s)) s3 (build-list 5 (lambda (i) (+ 20 (* 4 dia) (* i 2 dia))))))

(write-pict "worm2" (fake:place-image food (+ 30 dia dia) (+ 20 dia dia (* 4 dia) (* 8 dia)) s4))

;; --- worm3 

(define t1 mt)
(define t2 (foldl (lambda (x s) (fake:place-image worm x 30 s)) t1 (build-list 11 (lambda (i) (- 94 (* i 2 dia))))))

(write-pict "worm3" (fake:place-image (text "worm hit wall: 11" "roman" 9) 10 80 (fake:place-image food 96 (+ 30 dia dia) t2)))

