#lang scheme

(require (prefix-in fake: "image-fakes.ss") slideshow "write.ss" #;"pict-aux.ss")

(define mt (fake:empty-scene 100 100))

(define grid
  (foldl (lambda (i mt)
           (define x (* 10 i))
           (define y (* 10 i))
           (pin-line 
            (pin-line mt 
                      mt (lambda _ (values x 0))
                      mt (lambda _ (values x 99)))
            mt (lambda _ (values 0 y))
            mt (lambda _ (values 99 y))))
         mt
         (build-list 9 add1)))

(define c (fake:circle 5 "solid" "green"))

(write-pict "grid-scene" (fake:place-image c 45 75 grid))

