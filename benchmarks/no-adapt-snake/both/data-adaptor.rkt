#lang typed/racket

(require benchmark-util)

(define-type Snake (Pairof 'snake (Listof Any)))
(define-type World (Pairof 'world (Listof Any)))
(define-type Posn  (Pairof 'posn (Listof Any)))

(require/typed/check "data.rkt"
  (posn (-> Real Real Posn))
  (posn-x (-> Posn Real))
  (posn-y (-> Posn Real))
  ;; --
  (snake (-> Dir (NEListof Posn) Snake))
  (snake-dir (-> Snake Dir))
  (snake-segs (-> Snake (NEListof Posn)))
  ;; --
  (world (-> Snake Posn World))
  (world-snake (-> World Snake))
  (world-food (-> World Posn)))

(define-type (NEListof A) (Pairof A (Listof A)))
(define-type Dir (U "up" "down" "left" "right"))

(provide
 Snake snake snake-dir snake-segs
 World world world-snake world-food
 Posn  posn posn-x posn-y
 NEListof)
