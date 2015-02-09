#lang scheme

(require 2htdp/universe)

(define wheel-radius 5)
(define wheel-y-posn wheel-radius)
(define wheel-2-left (- (* 2 wheel-radius)))
(define wheel-2right (* 2 wheel-radius))

(define lwid (* 8 wheel-radius))
(define lhgt (* 2 wheel-radius))
(define uwid (* 4 wheel-radius))
(define uhgt wheel-radius)

(define wheel (circle wheel-radius "solid" "black"))
(define lbody (rectangle lwid lhgt "solid" "red"))
(define ubody (rectangle uwid uhgt "solid" "red"))

(define ubody+wheels 
  (overlay/xy (overlay/xy lbody wheel-2-left wheel-y-posn wheel) wheel-2right wheel-y-posn wheel))

(define car (overlay/xy ubody 0 (* 3/2 uhgt) ubody+wheels))

(define tree 
  (overlay (circle 10 'solid 'green) (nw:rectangle 2 20 'solid 'brown)))

(define mt
  (empty-scene 300 (+ wheel-radius (image-height car))))

(define y-car (+ (pinhole-y car) wheel-radius))

(define (render w)
  (place-image car w y-car (place-image tree 77 10 mt)))

(big-bang 0
          (on-tick add1)
          (on-draw render))

(map render '(50 100 150 200))
