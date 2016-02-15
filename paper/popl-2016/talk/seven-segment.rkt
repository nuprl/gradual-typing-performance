#lang racket/base

;; A small library for doing pictures of seven segment displays

(require data/bit-vector
         pict
         racket/class
         racket/draw
         racket/gui)

(provide digit-display
         number-display
	 string-display)

(define segment-width (make-parameter 60))
(define A-B-ratio (make-parameter 1.05))
(define width-height-ratio (make-parameter 5))
(define segment-spacing (make-parameter 2.5))

(define (segment-path horiz?)
  (define path (new dc-path%))
  (define sw
    (if horiz?
        (segment-width)
        (/ (segment-width) (A-B-ratio))))
  (define triangle-side-length
    (/ (segment-width) (* 2 (width-height-ratio))))
  (send path move-to 0 triangle-side-length)
  (send path line-to triangle-side-length 0)
  (send path line-to (- sw triangle-side-length) 0)
  (send path line-to sw triangle-side-length)
  (send path line-to
        (- sw triangle-side-length)
        (* 2 triangle-side-length))
  (send path line-to 
        triangle-side-length
        (* 2 triangle-side-length))
  (send path close)
  path)

(define (segment horiz? on?)
  (define path (segment-path horiz?))
  (define-values (l t w h) (send path get-bounding-box))
  (dc (λ (dc dx dy)
        (define old-brush (send dc get-brush))
        (define brush
          (if on?
              (send the-brush-list find-or-create-brush "tomato" 'solid)
              (send the-brush-list find-or-create-brush (make-color 60 60 60) 'solid)))
        (send dc set-brush brush)
        (send dc draw-path path dx dy)
        (send dc set-brush old-brush))
      w h))

(define (digit bv)
  (define (horiz on?) (segment #t on?))
  (define (vert on?)  (rotate (segment #f on?) (/ pi 2)))
  (define (get idx) (bit-vector-ref bv idx))
  (cc-superimpose
   (vc-append (+ (* 2 (segment-spacing))
                 (- (/ (segment-width) (A-B-ratio))
                    (/ (segment-width)
                       (width-height-ratio))))
              ;; A
              (horiz (get 0))
              ;; G
              (horiz (get 6))
              ;; D
              (horiz (get 3)))
   (vc-append (* 2 (segment-spacing))
              (hc-append (+ (* 2 (segment-spacing))
                            (- (segment-width)
                               (/ (segment-width)
                                  (width-height-ratio))))
                         ;; F
                         (vert (get 5))
                         ;; B
                         (vert (get 1)))
              (hc-append (+ (* 2 (segment-spacing))
                            (- (segment-width)
                               (/ (segment-width)
                                  (width-height-ratio))))
                         ;; E
                         (vert (get 4))
                         ;; C
                         (vert (get 2))))))

(define (digit-display/bv bv)
  (define the-digit (digit bv))
  (define w (pict-width the-digit))
  (define h (pict-height the-digit))
  (cc-superimpose (colorize (filled-rectangle (+ w 30) (+ h 30))
                            "black")
                  the-digit))

(define (digit-display digit)
  (digit-display/bv (cdr (assoc digit number-table))))

(define (number-display num)
  (apply hc-append
         5
         (for/list ([dig (number->string num)])
           (digit-display (string->number (string dig))))))

(define (string-display str)
  (apply hc-append
         5
         (for/list ([dig str])
           (digit-display (string->number (string dig))))))

(define number-table
  `((0 . ,(bit-vector #t #t #t #t #t #t #f))
    (1 . ,(bit-vector #f #t #t #f #f #f #f))
    (2 . ,(bit-vector #t #t #f #t #t #f #t))
    (3 . ,(bit-vector #t #t #t #t #f #f #t))
    (4 . ,(bit-vector #f #t #t #f #f #t #t))
    (5 . ,(bit-vector #t #f #t #t #f #t #t))
    (6 . ,(bit-vector #t #f #t #t #t #t #t))
    (7 . ,(bit-vector #t #t #t #f #f #f #f))
    (8 . ,(bit-vector #t #t #t #t #t #t #t))
    (9 . ,(bit-vector #t #t #t #f #f #t #t))))

(module+ main
  (show-pict (number-display 357)))
