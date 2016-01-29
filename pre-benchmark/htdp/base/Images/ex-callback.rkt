;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-callback) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH  100)
(define HEIGHT 100)
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define BUTTON-WIDTH  80)
(define BUTTON-HEIGHT 20)
(define BUTTON (rectangle BUTTON-WIDTH BUTTON-HEIGHT 'solid 'red))

(define X-BUTTON 10)
(define Y-BUTTON 10)
(define SCENE (place-image/align BUTTON X-BUTTON Y-BUTTON 'left 'top BACKGROUND))

(define X-TEXT X-BUTTON)
(define Y-TEXT (+ Y-BUTTON BUTTON-HEIGHT 10))

;; -----------------------------------------------------------------------------
;; Image N N -> [N N -> Boolean]
;; create a function of two natural numbers that
;; determines whether a mouse click is in the extent of button placed at (x,y)

(check-expect ((make-callback BUTTON X-BUTTON Y-BUTTON) 
               (+ X-BUTTON 1) (+ Y-BUTTON 1)) 
              true)
(check-expect ((make-callback BUTTON X-BUTTON Y-BUTTON) 
               (+ X-BUTTON BUTTON-WIDTH 1) (+ Y-BUTTON 1)) 
              false)
(check-expect ((make-callback BUTTON X-BUTTON Y-BUTTON) 
               (+ X-BUTTON 1) (+ Y-BUTTON BUTTON-HEIGHT 1)) 
              false)

(define (make-callback button X0 Y0)
  (local ((define WIDTH (+ X0 (image-width button)))
          (define HIGHT (+ Y0 (image-height button))))
    (lambda (x y)
      (and (< X0 x WIDTH) (< Y0 y HIGHT)))))

;; -----------------------------------------------------------------------------


;; Image Image N N -> N
;; how many times is the button pressed
(define (main SCENE BUTTON X-BUTTON Y-BUTTON)
  (local ((define callback (make-callback BUTTON X-BUTTON Y-BUTTON)))
    (big-bang 
     x
     [to-draw
      (lambda (w)
        (place-image/align (text (number->string w) 22 'blue) X-TEXT Y-TEXT 'left 'top SCENE))]
     [on-mouse 
      (lambda (w x y me)
        (if (and (mouse=? "button-down" me) (callback x y))
            (+ w 1)
            w))])))