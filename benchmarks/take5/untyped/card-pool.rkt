#lang racket

;; a representation of the complete deck of cards 

(require "card.rkt" "basics.rkt" "../base/utility.rkt")

(types Card Bulls)

(provide
 (type CardPool [Instance CardPool%])
 
 (type CardPool%
       (Class
        (draw-card
         ;; effect: pick and return one card from the pool of cards
         (-> Card))
        (draw-hand
         ;; effect: pick and return HAND cards from the pool of cards
         (-> Hand))))
 
 (type Hand [Listof Card]) ;; of size Hand
 
 ;;   { [Listof X] -> [Listof X] }
 ;;   { -> Bulls }
 ;; -> CardPool
 ;; create and organize the pool of cards in a random order so that
 ;; the dealer can hand cards to players and create the initial deck
 ;;
 ;; so we can keep things deterministic 
 ;; the first optional argument is a shuffle algorithm for lists
 ;; the second optional argument generates bulls 
 create-card-pool)

;; ---------------------------------------------------------------------------------------------------
(module+ test (require rackunit))

(define (create-card-pool (shuffle shuffle) (random-bulls random-bulls))
  (new card-pool% (shuffle shuffle) (random-bulls random-bulls)))

;; -> Bulls
;; pick a random number of BULLS 
(define (random-bulls)
  (random MIN-BULL (+ MAX-BULL 1)))

(define card-pool%
  (class object%
    (init-field (shuffle shuffle) (random-bulls random-bulls))
    (super-new)
    
    ;; [Listof Card]
    (define my-cards
      (shuffle (build-list FACE (lambda (i) (card (+ i 1) (random-bulls))))))
    
    (define/public (draw-card)
      (begin0 (first my-cards)
              (set! my-cards (rest my-cards))))
    
    (define/public (draw-hand)
      (build-list HAND (lambda (_) (draw-card))))))


(module+ test
  (require "card.rkt" "basics.rkt")
  
  (check-equal? (let ((cp (create-card-pool values (lambda () MIN-BULL))))
                  (send cp draw-card))
                (card 1 MIN-BULL))
  
  (check-equal? (let ((cp (create-card-pool values (lambda () MIN-BULL))))
                  (send cp draw-hand))
                (build-list HAND (lambda (i) (card (+ i 1) MIN-BULL)))))
