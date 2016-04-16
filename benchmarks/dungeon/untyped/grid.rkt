#lang racket

(require math/array
         "cell.rkt")

(provide (all-defined-out))

;; a Grid is a math/array Mutable-Array of cell%
;; (mutability is required for dungeon generation)

;; parses a list of strings into a grid, based on the printed representation
;; of each cell
(define (parse-grid los)
  (for*/array #:shape (vector (length los)
                              (apply max (map string-length los)))
              #:fill (new void-cell%)
              ([s (in-list los)]
               [c (in-string s)])
     (new (char->cell% c))))

(define (show-grid g)
  (with-output-to-string
    (lambda ()
      (for ([r (in-array-axis g)])
        (for ([c (in-array r)])
          (display (send c show)))
        (newline)))))

(define (grid-height g)
  (match-define (vector rows cols) (array-shape g))
  rows)
(define (grid-width g)
  (match-define (vector rows cols) (array-shape g))
  cols)
(define (within-grid? g pos)
  (and (<= 0 (vector-ref pos 0) (sub1 (grid-height g)))
       (<= 0 (vector-ref pos 1) (sub1 (grid-width  g)))))
(define (grid-ref g pos)
  (and (within-grid? g pos)
       (array-ref g pos)))

(define (left pos [n 1])
  (vector (vector-ref pos 0)
          (- (vector-ref pos 1) n)))
(define (right pos [n 1])
  (vector (vector-ref pos 0)
          (+ (vector-ref pos 1) n)))
(define (up pos [n 1])
  (vector (- (vector-ref pos 0) n)
          (vector-ref pos 1)))
(define (down pos [n 1])
  (vector (+ (vector-ref pos 0) n)
          (vector-ref pos 1)))


(module+ test
  (require rackunit)

  (define (parse-and-show los) (show-grid (parse-grid los)))
  (define (render-grid g) (string-join g "\n" #:after-last "\n"))

  (define g1
    '(" "))
  (check-equal? (parse-and-show g1) " \n")

  (define g2
    '("**********"
      "*        *"
      "*        *"
      "*        *"
      "**********"))
  (check-equal? (parse-and-show g2) (render-grid g2))

  (define g3 ; padding should work
    '("**********"
      "*        *"
      "*        *"
      "*        *"
      "*****"))
  (define g3*
    '("**********"
      "*        *"
      "*        *"
      "*        *"
      "*****....."))
  (check-equal? (parse-and-show g3) (render-grid g3*))

  (define g2* (parse-grid g2))
  (check-true (within-grid? g2* '#(0 0)))
  (check-true (within-grid? g2* '#(0 1)))
  (check-true (within-grid? g2* '#(1 0)))
  (check-true (within-grid? g2* '#(4 4)))
  (check-false (within-grid? g2* '#(0 10)))
  (check-false (within-grid? g2* '#(5 0)))
  (check-false (within-grid? g2* '#(5 10)))
  )
