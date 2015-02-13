;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spaceinvaders3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
; Space Invaders

;
; Canvas
;
(define HEIGHT 800)
(define WIDTH 500)
(define BGCOLOR "white")

(define BACKGROUND (empty-scene WIDTH HEIGHT BGCOLOR))

;
; Objects
;
; UFO

(define SAUCER (ellipse 60 12 "solid" "lightsteelblue"))
(define ORB (circle 15 "solid" "gray"))

(define UFO
  (overlay ORB SAUCER))


(define UFO-HEIGHT (image-height UFO))
(define UFO-WIDTH (image-width UFO))

;
; missile
(define MISSILE-COLOR "red")

(define MISSILE (isosceles-triangle 15 20 "solid" MISSILE-COLOR))

;
; tank

(define TANK-COLOR "seagreen")

(define TANKBODY-HEIGHT 30)
(define TANKTRI-LEFT (rotate 45 (triangle/ssa TANKBODY-HEIGHT TANKBODY-HEIGHT 90 "solid" TANK-COLOR)))
(define TANKTRI-RIGHT (flip-horizontal TANKTRI-LEFT))
(define TANKBODY (rectangle (* 3/2 TANKBODY-HEIGHT) TANKBODY-HEIGHT "solid" TANK-COLOR))

(define TREADCOLOR "darkslategray")

(define WHEEL (circle (* 1/3 TANKBODY-HEIGHT) "solid" TREADCOLOR))
(define CHAINS (rectangle (* 3/2 TANKBODY-HEIGHT) (* 2/3 TANKBODY-HEIGHT) "solid" TREADCOLOR))
(define TREADS (beside
                WHEEL
                CHAINS
                WHEEL))

(define TANK-UNARMED (overlay/offset TREADS 0 (* -1/3 TANKBODY-HEIGHT)
                                     (beside TANKTRI-LEFT TANKBODY TANKTRI-RIGHT)))

(define TANK-ARMED (above MISSILE TANK-UNARMED))

;
; other graphical constants
;

(define UFO-XMIN (* 1/2 UFO-WIDTH))
(define UFO-XMAX (- WIDTH UFO-XMIN))
(define UFO-RANGE (- WIDTH UFO-WIDTH)) ; UFO horizontal position is in [UFO-XMIN, UFO-XMAX]

(define TANK-XMIN (* 1/2 (image-width TANK-ARMED)))
(define TANK-XMAX (- WIDTH TANK-XMIN))


;
; initial scene
;


(define TANK-ARMED-ALTITUDE (- HEIGHT (* 1/2 (image-height TANK-ARMED))))
(define TANK-UNARMED-ALTITUDE (- HEIGHT (* 1/2 (image-height TANK-UNARMED))))

(define UFO-INIT-ALTITUDE (* 1/2 (image-height UFO))) 

(define INIT-SCENE
  (place-image UFO UFO-XMAX UFO-INIT-ALTITUDE
               (place-image TANK-ARMED TANK-XMIN TANK-ARMED-ALTITUDE
                            BACKGROUND)))


(define KA-BOOM!
  (star-polygon 20 10 3 "solid" "yellow"))

;
; game constants
;

(define UFO-DESCENT-VELOCITY 2)
(define MISSILE-VELOCITY (* -5/2 UFO-DESCENT-VELOCITY))
(define INIT-TANK-VELOCITY 3)
(define RANDOM-SEED (/ HEIGHT 1))  ; there is a 1 in RANDOM-SEED chance the UFO will jump horizontally
(define CLOCK-SPEED 1/28)
(define TEXT-SIZE (floor (/ WIDTH 15)))


; world state SIGS
(define-struct sigs (aim fired))
;
; where aim is a structure with fields (posn tank) used before
; the missile is fired.
;
; posn represents the position of the flying saucer
; tank is a structure with fields (x, velocity), both numbers
; x is in the interval [TANK-XMIN, TANK-XMAX]
; velocity is a Number, positive values represent motion to the right
;
(define-struct aim (ufo tank))

(define-struct ufo (x y))
(define-struct tank (x velocity))
;
;
; and fired is a structure with three fields (ufo tank missile) used after
; the missile is fired.
;
; the ufo field is as before
; the tank field is as before
; the missile field represents the position of the missile
;
(define-struct fired (ufo tank missile))

(define-struct missile (x y))


;
;
; play
;


(define (play xloc)
  (big-bang (make-aim
             (make-ufo UFO-XMAX UFO-INIT-ALTITUDE)
             (make-tank xloc INIT-TANK-VELOCITY))
            (on-tick si-clock CLOCK-SPEED)
            (to-draw si-render)
            (on-key si-actions)
            (stop-when si-endgame? finish-up)))

(define (finish-up final-ws)
  (if (fired? final-ws)
      (if (close-enough? final-ws)
          (render-win (fired-ufo final-ws) (fired-tank final-ws))
          (render-missed (fired-ufo final-ws) (fired-tank final-ws) (fired-missile final-ws)))
      (render-alien-invasion (aim-ufo final-ws) (aim-tank final-ws))))


(define (render-win u t)
  (above (tank-render t true
                      (place-image KA-BOOM!
                                   (ufo-x u)
                                   (ufo-y u)
                                   BACKGROUND))
         (text "Huzzah! You defeated the aliens." TEXT-SIZE "green")))

(define (render-missed u t m)
  (above (tank-render t true
                      (ufo-render u (missile-render m BACKGROUND)))
         (text "You missed.  Resistance is futile.  You will be assimilated." TEXT-SIZE "red")))

(define (render-alien-invasion u t)
  (above (tank-render t false (ufo-render u BACKGROUND))
         (text "The aliens have landed! You will be assimilated." TEXT-SIZE "red")))


;
;
; si-clock
; signature: sigs -> sigs
;
(define (si-clock s)
  (cond [(aim? s) (make-aim (make-ufo
                             (if (zero? (random RANDOM-SEED))
                                 (+ UFO-XMIN (random UFO-RANGE))
                                 (ufo-x (aim-ufo s)))
                             (+ (ufo-y (aim-ufo s)) UFO-DESCENT-VELOCITY))
                            (make-tank (move-tank (+ (tank-x (aim-tank s)) (tank-velocity (aim-tank s))))
                                       (tank-velocity (aim-tank s))))]
        [(fired? s) (make-fired (make-ufo
                                 (if (zero? (random RANDOM-SEED))
                                     (+ UFO-XMIN (random UFO-RANGE))
                                     (ufo-x (fired-ufo s)))
                                 (+ (ufo-y (fired-ufo s)) UFO-DESCENT-VELOCITY))
                                (make-tank (move-tank (+ (tank-x (fired-tank s)) (tank-velocity (fired-tank s))))
                                           (tank-velocity (fired-tank s)))
                                (make-missile (missile-x (fired-missile s)) (+ (missile-y (fired-missile s)) MISSILE-VELOCITY)))]))


(define (move-tank newx)
  (cond [(<= newx TANK-XMIN) TANK-XMIN]
        [(>= newx TANK-XMAX) TANK-XMAX]
        [else newx]))

;
;
; si-render
; signature: sigs -> {draw canvas}
;
(define (si-render s)
  (cond
    [(aim? s) (tank-render (aim-tank s) false
                           (ufo-render (aim-ufo s)
                                       BACKGROUND))]
    [(fired? s) (tank-render (fired-tank s) true
                             (ufo-render (fired-ufo s)
                                         (missile-render (fired-missile s) BACKGROUND)))]))

(define (tank-render t launched image)
  (if launched
      (place-image TANK-UNARMED (tank-x t) TANK-UNARMED-ALTITUDE image)
      (place-image TANK-ARMED (tank-x t) TANK-ARMED-ALTITUDE image)))

(define (ufo-render u image)
  (place-image UFO (ufo-x u) (ufo-y u) image))

(define (missile-render m image)
  (place-image MISSILE (missile-x m) (missile-y m) image))


;
;
; si-actions
;
(define (si-actions s keystroke)
  (cond [(string=? keystroke " ") (launch-missile s)]
        [(string=? keystroke "left" ) (if (aim? s)
                                          (make-aim (aim-ufo s) (tank-go-left (aim-tank s)))
                                          (make-fired (fired-ufo s) (tank-go-left (fired-tank s)) (fired-missile s)))]
        
        [(string=? keystroke "right") (if (aim? s)
                                          (make-aim (aim-ufo s) (tank-go-right (aim-tank s)))
                                          (make-fired (fired-ufo s) (tank-go-right (fired-tank s)) (fired-missile s)))]
        [(string=? keystroke "down") (if (aim? s)
                                         (make-aim (aim-ufo s) (tank-stop (aim-tank s)))
                                         (make-fired (fired-ufo s) (tank-stop (fired-tank s)) (fired-missile s)))]
        [else s]))


(define (launch-missile s)
  (if (aim? s)
      (make-fired
       (aim-ufo s)
       (aim-tank s)
       (make-missile (tank-x (aim-tank s))
                     (- HEIGHT (image-height TANK-UNARMED))))
      s))


(define (tank-go-right t)
  (make-tank (tank-x t) INIT-TANK-VELOCITY))

(define (tank-go-left t)
  (make-tank (tank-x t) (* -1 INIT-TANK-VELOCITY)))

(define (tank-stop t)
  (make-tank (tank-x t) 0))

;
;
; si-endgame?
;
(define (si-endgame? s)
  (cond [(ufo-landed? s) true]
        [(missile-too-high? s) true]
        [(close-enough? s) true]
        [else false]))


(define (ufo-landed? s)
  (cond [(aim? s) (>= (ufo-y (aim-ufo s)) (- HEIGHT UFO-INIT-ALTITUDE))]
        [(fired? s) (>= (ufo-y (fired-ufo s)) (- HEIGHT UFO-INIT-ALTITUDE))]))

(define (missile-too-high? s)
  (cond [(aim? s) false]
        [(fired? s) (< (missile-y (fired-missile s)) (ufo-y (fired-ufo s)))]))

(define (close-enough? s)
  (cond [(aim? s) false]
        [(fired? s) (missile-between? (missile-x (fired-missile s)) (missile-y (fired-missile s))
                                      (ufo-x (fired-ufo s)) (ufo-y (fired-ufo s))
                                      UFO-WIDTH
                                      UFO-HEIGHT)]))

(define (missile-between? mx my ux uy deltax deltay)
  (and (and (<= my (+ uy deltax))
            (>= my (- uy deltay)))
       (and (<= mx (+ ux deltax))
            (>= mx (- ux deltay)))))


