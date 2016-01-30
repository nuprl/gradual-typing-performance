#lang typed/racket

;; The module provides a timer mixing for world and universe.

;; The interface ensures that super class provides start and stop method, 
;; BUT if fails to ensure that the class comes with a _tick_ field. 
;; plus a call back for clock ticks. The super-init call sets the 
;; tick field, which the super-class uses to define the callback.


(require typed/racket/gui "check-aux.rkt" "stop.rkt" "world-type.rkt")

(define-type On-Tick
  (U (List (World -> World) Natural Natural)
     (List (World -> World) Natural)
     (World -> World)))

(provide clock-mixin Start-Stop<%>)

(define-type Start-Stop<%>
  (Class [start! (-> Void)]
         (augment [ptock (-> Void)])
         [pptock (World -> (U stop-the-world World))]
         [name-of-tick-handler (-> (U Symbol String))]
         [stop! ((U exn World) -> Void)]))

(: clock-mixin (All (r #:row)
                 ((Class #:row-var r #:implements Start-Stop<%>)
                  ->
                  (Class #:row-var r #:implements Start-Stop<%>
                         (init-field [on-tick (Option On-Tick) #:optional])
                         (field [rate Real]
                                [limit (Option Natural)]
                                [tick (World -> World)]
                                [tick# Natural]
                                [timer (Instance Timer%)])))))
(define (clock-mixin cls)
  (class cls
    (inherit ptock)
    (init-field [on-tick #f])
    (field [rate  0]
           [limit #f]
           [tick  values]
           [tick# 0]
           [timer (new timer% [notify-callback (lambda () (set! tick# (+ tick# 1)) (ptock))])])
    (match on-tick
      [`(,handler ,r ,l) 
       (set! limit l)
       (set! rate r)
       (set! tick handler)]
      [`(,handler ,r) 
       (set! rate r)
       (set! tick handler)]
      [(? procedure? handler)
       (set! rate RATE)
       (set! tick handler)]
      [else (void)])
    (define/override (start!)
      (unless (<= rate 0)
        (send timer start (assert (number->integer (* 1000 rate) 'big-bang/universe 'clock-rate)
                                  exact-nonnegative-integer?)))
      (super start!))
    (define/override (stop! w)
      (send timer stop)
      (super stop! w))
    (define/override (pptock w)
      (define limit* limit)
      (if (and limit* (> tick# limit*))
          (make-stop-the-world w)
          (tick w)))
    (define/override (name-of-tick-handler)
      (assert (object-name tick) symbol?))
    (super-new)))
