#lang typed/racket

;; provide basic elements for game pad clause in big-bang: the icon, pad-event?

(require typed/racket/gui racket/runtime-path)

(provide
  ;; bitmap
  game-pad
  ;; is the given key-event also a pad-event? 
  pad-event? 
  ;; are the two pad-events equal? 
  pad=?
  )
(: pad-event? : Any -> Boolean)
(: pad=? : String String -> Boolean)

;; ---------------------------------------------------------------------------------------------------

(define-runtime-path gamepad-path "gamepad.png")
(define game-pad  (read-bitmap gamepad-path 'png/alpha #f #t))
(unless (send game-pad ok?)
  (error 'big-bang "the game pad icon isn't available; please report error"))

(define pad-buttons
  '("up"    "w" 
    "down"  "s" 
    "left"  "a"
    "right" "d"
    " "
    "shift" "rshift"))

(define (pad-event? ke)
  (pair? (member ke pad-buttons)))

(define (pad=? ke kq)
  (and (pad-event? ke) (pad-event? kq) (string=? ke kq)))
