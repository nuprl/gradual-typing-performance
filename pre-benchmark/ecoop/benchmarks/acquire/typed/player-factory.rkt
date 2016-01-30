#lang typed/racket 

;; ---------------------------------------------------------------------------------------------------
;; a factory for creating lists of random and ordered players plus players with specific problems

(require "require-typed-check.rkt" (except-in "typed-wrapper.rkt" player?))

(provide random-players
         ordered-players
         merge-bad-player
         keep-bad-player
         end-bad-player
         receive-bad-player
         setup-bad-player
         inform-bad-player
         inf-loop-player)

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

(require/typed/check "player.rkt"
                     [player? (-> Any Boolean)]
                     [create (-> String Strategy (Instance Player%))])

(require/typed "strategy.rkt" [ordered-s Strategy] [random-s Strategy])

(: ordered-players (Integer -> (Listof (Instance Player%))))
(define (ordered-players n) (players ordered-s n))
(: random-players (Integer -> (Listof (Instance Player%))))
(define (random-players n) (players random-s n))
(: inf-loop-player (Integer -> (Instance Player%)))
(define (inf-loop-player n)
  (: S Strategy)
  (define (S x) (if (> n 0) (begin (set! n (- n 1)) (ordered-s x)) (let: L : (Values (Option Tile) (Option Hotel) Shares-Order) () (L))))
  (create (format "inf loop after ~a" n) S))

(: merge-bad-player (-> (Instance Merger%)))
(define (merge-bad-player) (new merger%))
(: setup-bad-player (-> (Instance Setup%)))
(define (setup-bad-player) (new setup%))
(: inform-bad-player (-> (Instance Inform%)))
(define (inform-bad-player) (new inform%))
(: receive-bad-player (-> (Instance Receive%)))
(define (receive-bad-player) (new receive%))
(: end-bad-player (-> (Instance The-End%)))
(define (end-bad-player) (new the-end%))
(: keep-bad-player (-> (Instance Keep%)))
(define (keep-bad-player) (new keep%))

(: players (Strategy Integer -> (Listof (Instance Player%))))
(define (players S n)
  (for/list: : (Listof (Instance Player%)) ((name '("a" "b" "c" "d" "e" "f")) (i (in-range n))) (create name S)))

(define-type Base% 
  (Class #:implements Player%
         (init-field  [name String #:optional] [choice Strategy #:optional])))

(: base% Base%)  
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

(define-type Setup%
  (Class #:implements Base%
         (init [choice Strategy #:optional])))

(: setup% Setup%)         
(define setup%
  (class base% 
    (super-new (name "diverges on set up"))
    (define/override (setup ts) (let: L : Void () (L)))))

(define-type Inform%
  (Class #:implements Base%
         (init [choice Strategy #:optional])))

(: inform% Inform%)
(define inform%
  (class base%
    (super-new (name "diverges on inform"))
    (define/override (inform s) (let: L : Void () (L)))))

(define-type Receive%
  (Class #:implements Base%
         (init [choice Strategy #:optional])))

(: receive% Receive%)
(define receive%
  (class base% 
    (super-new (name "diverges on receive"))
    (define/override (receive-tile t) (let: L : Void () (L)))))

(define-type The-End%
  (Class #:implements Base%
         (init [choice Strategy #:optional])))

(: the-end% The-End%)
(define the-end%
  (class base% 
    (super-new (name "diverges on end game"))
    (define/override (the-end s c) (let: L : Void () (L)))))

(define-type Keep%
  (Class #:implements Base%
         (init [choice Strategy #:optional])))

(: keep% Keep%)
(define keep%
  (class base% 
    (super-new (name "diverges on keep"))
    (define/override (keep l) (let:  L : (Listof Boolean) () (L)))))

(define-type Merger%
  (Class #:implements Base%))

(: merger% Merger%)
(define merger%
  (class base% 
    (super-new
     (name "diverges after a keep was called once")
     (choice (lambda (x) (if *keep (let: L : (Values (Option Tile) (Option Hotel) Shares-Order) () (L)) (ordered-s x)))))
    (: *keep Boolean)
    (define *keep #f)
    (define/override (keep l) (set! *keep #t) (super keep l))))
