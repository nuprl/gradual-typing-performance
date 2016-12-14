#lang typed/racket/base

;; representing the deck of STACKS stack on the table 

(provide
 ;; CardPool -> Deck 
 create-deck)

;; -----------------------------------------------------------------------------

(require
  typed/racket/class
  "basics-types.rkt"
  "card-adapted.rkt"
  "card-pool-types.rkt"
  "deck-types.rkt"
  "stack-types.rkt"
  racket/list
  require-typed-check
)

(require/typed/check "basics.rkt"
  (FACE Natural)
  (STACKS Natural))

(require/typed/check "stack.rkt"
  (bulls  (-> Stack Natural)))

;; ---------------------------------------------------------------------------------------------------

(: create-deck (-> CardPool Deck))
(define (create-deck card-pool)
  (define deck% (for-player (for-dealer base-deck%)))
  (define cards (build-list STACKS (lambda (_) (send card-pool draw-card))))
  (new deck% [cards0 cards]))

;; Class[my-stacks field] -> Class[my-stacks field and fewest-bulls method]
(: for-player (-> DealerDeck% Deck%))
(define (for-player deck%)
  (class deck%
    (inherit-field my-stacks)
    (super-new)

    (define/public (fewest-bulls)
      (define stacks-with-bulls : (Listof (List Stack Natural))
        (for/list : (Listof (List Stack Natural))
                  ((s my-stacks))
          (list s (bulls s))))
      (first (argmin (lambda ([l : (List Stack Natural)]) (second l)) stacks-with-bulls)))))

;; Class[cards0 field] -> Class[fit, push, replace & larger-than-some-top-of-stacks? methods]
(: for-dealer (-> BaseDeck% DealerDeck%))
(define (for-dealer deck%)
  (class deck%
    (inherit-field cards0)
    (inherit-field my-stacks)
    (super-new)

    ;; [Listof Stack]
    (set-field! my-stacks this (map (lambda ([c : Card]) (list c)) cards0))
    ;(field [my-stacks

    (define/public (fit c)
      (: distance (-> (Listof Card) Real))
      (define (distance stack)
        (define d (first stack))
        (if (>-face c d) (--face c d) (+ FACE 1)))
      (argmin distance my-stacks))

    (define/public (push c)
      (define s0 (fit c))
      (void (replace-stack (first s0) c)))

    (define/public (replace s c)
      (replace-stack (first s) (list c)))

    (define/public (replace-stack top0 c)
      (define result : Natural 0)
      (set! my-stacks 
            (for/list : (Listof Stack) ((s : Stack my-stacks))
              (cond
                [(equal? (first s) top0)
                 (set! result (bulls s))
                 (cast
                  (if (cons? c)
                   c
                   (cons c s))
                  Stack)]
                [else s])))
      result)

    (define/public (larger-than-some-top-of-stacks? c)
      (for/or ((s my-stacks))
        (>-face c (first s))))))

;; Class[cards0 field]
(: base-deck% BaseDeck%)
(define base-deck%
  (class object%
    (init-field
     ;; [Listof Card]
     ;; the tops of the initial stacks (for a round)
     cards0)

    (field (my-stacks '()))

    (super-new)))
