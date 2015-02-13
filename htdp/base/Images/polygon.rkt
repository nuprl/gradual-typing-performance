#lang scheme

(require (prefix-in fake: "image-fakes.ss") slideshow "write.ss" #;"pict-aux.ss")

(define mt (fake:empty-scene 100 100))

(define width 10)
(define divisions 10)
(define factor 2)

(define (add-line mt x0 y0 x1 y1)
  (pin-line mt 
            mt (lambda _ (values (* factor x0) (* factor y0)))
            mt (lambda _ (values (* factor x1) (* factor y1)))))

(define (add-dot im x y r)
  (define c (fake:circle r "solid" "green"))
  (fake:place-image c (- x (quotient r 2)) (- y (quotient r 2)) im))

(define grid mt)

(define-struct posn (x y))

(define tri (list (make-posn 20 10) (make-posn 20 20) (make-posn 10 20)))
(define po1 (cons (make-posn 10 10) tri))
(define po2 (cons (make-posn 10 5) (cons (make-posn 10 10) tri)))

(define triangle 
  (local ((define p1 
            (foldl (lambda (p im)
                     (define x (* factor (posn-x p)))
                     (define y (* factor (posn-y p)))
                     (add-dot im x y 2))
                   grid
                   tri))
          (define p2 (add-line p1 20 10 20 20))
          (define p3 (add-line p2 20 20 10 20))
          (define p4 (add-line p3 10 20 20 10)))
    p4))

(define polygon 
  (local ((define p1 
            (foldl (lambda (p im)
                     (define x (* factor (posn-x p)))
                     (define y (* factor (posn-y p)))
                     (add-dot im x y 2))
                   grid
                   po1))
          (define p2 (add-line p1 20 10 20 20))
          (define p3 (add-line p2 20 20 10 20))
          (define p4 (add-line p3 10 20 20 10))
          (define p5 (add-line p4 10 10 20 10)))
    p5))

(define polygon2
  (local ((define p1 
            (foldl (lambda (p im)
                     (define x (* factor (posn-x p)))
                     (define y (* factor (posn-y p)))
                     (add-dot im x y 2))
                   grid
                   po2))
          (define p2 (add-line p1 20 10 20 20))
          (define p3 (add-line p2 20 20 10 20))
          (define p4 (add-line p3 10 20 20 10))
          (define p5 (add-line p4 10 10 20 10))
          (define p6 (add-line p5 10 5 10 10)))
    p6))

(write-pict "triangle" triangle)
(write-pict "polygon" polygon)
(write-pict "polygon2" polygon2)

