#lang racket 

;; ---------------------------------------------------------------------------------------------------
;; a factory for creating lists of random and ordered players plus players with specific problems

(require (only-in "player.rkt" player?) "admin-intf.rkt")

(provide 
 ;(contract-out 
  random-players ;(-> natural-number/c (listof player?)))
  ordered-players ;(-> natural-number/c (listof player?)))
  
  merge-bad-player ;(-> (instanceof/c player/c)))
  keep-bad-player ;(-> (instanceof/c player/c)))
  end-bad-player ;(-> (instanceof/c player/c)))
  receive-bad-player ;(-> (instanceof/c player/c)))
  setup-bad-player ;(-> (instanceof/c player/c)))
  inform-bad-player ;(-> (instanceof/c player/c)))
  inf-loop-player #;(-> natural-number/c player?))

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

(require "strategy.rkt" "player.rkt")

(define (ordered-players n) (players ordered-s n))
(define (random-players n) (players random-s n))
(define (inf-loop-player n)
  (define (S . x) (if (> n 0) (begin (set! n (- n 1)) (apply ordered-s x)) (let L () (L))))
  (create (format "inf loop after ~a" n) S))

(define (merge-bad-player) (new merger%))
(define (setup-bad-player) (new setup%))
(define (inform-bad-player) (new inform%))
(define (receive-bad-player) (new receive%))
(define (end-bad-player) (new the-end%))
(define (keep-bad-player) (new keep%))

;; Strategy Nat -> [Listof ExternalPlayer]
(define (players S n)
  (for/list ((name '("a" "b" "c" "d" "e" "f")) (i (in-range n))) (create name S)))

(define base% 
  (class object% 
    (init-field 
     (name "diverges on inform") 
     (choice ordered-s))
    (super-new)
    
    (define/public (go a) (send a sign-up name this))
    (define/public (setup s) (void))
    (define/public (take-turn turn) (choice turn))
    (define/public (keep acquired-hotels) (map (lambda (x) #t) acquired-hotels))
    (define/public (receive-tile t) (void)) 
    (define/public (inform s) (void)) ;; <---- infinite loop 
    (define/public (the-end s sc) (void))))

(define setup%
  (class base% 
    (super-new (name "diverges on set up"))
    (define/override (setup ts) (let L () (L)))))

(define inform%
  (class base%
    (super-new (name "diverges on inform"))
    (define/override (inform s) (let L () (L)))))

(define receive%
  (class base% 
    (super-new (name "diverges on receive"))
    (define/override (receive-tile t) (let L () (L)))))

(define the-end%
  (class base% 
    (super-new (name "diverges on end game"))
    (define/override (the-end s c) (let L () (L)))))

(define keep%
  (class base% 
    (super-new (name "diverges on keep"))
    (define/override (keep l) (let L () (L)))))

(define merger%
  (class base% 
    (super-new
     (name "diverges after a keep was called once")
     (choice (lambda x (if *keep (let L () (L)) (apply ordered-s x)))))
    (define *keep #f)
    (define/override (keep l) (set! *keep #t) (super keep l))))
