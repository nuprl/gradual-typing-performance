#|
possible to disable dragging but still allow double-clicking?
possible to remap single click (instead of double click)?
|#

#lang typed/racket
(require/typed "show-scribbling.rkt" [show-scribbling (Module-Path String -> (-> Void))])

(require/typed
 "region.rkt"
 [make-button-region 
  (Real Real Real Real (U String #f) (U ((Listof (Instance Card%)) -> Any) #f) -> Region)])

(require typed/racket/class (except-in typed/racket/gui pasteboard%) racket/unit string-constants 
         "classes.rkt" "card-class.rkt" "make-cards.rkt" "utils.rkt" "typed-base.rkt")

(define make-table
  (lambda ([title : String "Cards"][w : Nonnegative-Real 7][h : Nonnegative-Real 3])
    (make-object table% title w h)))

(: make-deck (-> (Listof (Instance Card%))))
(define (make-deck)
  (map (λ ([l : (Instance Card%)]) (send l copy)) deck-of-cards))

(define table (make-table "Aces" 6 5))

(make-object button% (string-constant help-menu-label) table
  (let ([show-help (show-scribbling '(lib "games/scribblings/games.scrbl")
                                    "aces")])
    (lambda x (show-help))))

(: draw-pile (U Null (Listof (Instance Card%))))
(define draw-pile null)


(define card-height (send (car (make-deck)) card-height))
(define card-width (send (car (make-deck)) card-width))
(define region-height (send table table-height))

;; space between cards in the 4 stacks
(define card-space 30)

(define-struct: stack ([x : Real] 
                       [y : Real] 
                       [cards : (Listof (Instance Card%))]) #:mutable)

(define-type Stack stack)

(: get-x-offset (Real -> Real))
(define (get-x-offset n)
  (let* ([table-width (send table table-width)]
         [stack-spacing 7]
         [num-stacks 5]
         [all-stacks-width (+ (* num-stacks card-width)
                              (* (- num-stacks 1) stack-spacing))])
    (+ (- (/ table-width 2) (/ all-stacks-width 2))
       (* n (+ card-width stack-spacing)))))

(define draw-pile-region
  (make-button-region
   (get-x-offset 0)
   0
   card-width
   region-height ; card-height
   #f
   #f))

;(: stacks (Listof Stack))
(define stacks
  (list (make-stack (get-x-offset 1) 0 null)
        (make-stack (get-x-offset 2) 0 null)
        (make-stack (get-x-offset 3) 0 null)
        (make-stack (get-x-offset 4) 0 null)))

(define-struct: state ([draw-pile : (Listof (Instance Card%))] 
                       [stacks : (Listof (Listof (Instance Card%)))]))
                                  
(define-type State state)

(: extract-current-state (-> State))
(define (extract-current-state)
  (make-state (copy-list draw-pile)
              (map (lambda: ([x : Stack]) 
                     (copy-list (stack-cards x))) stacks)))

(: copy-list ((Listof (Instance Card%)) -> (Listof (Instance Card%))))
(define (copy-list l) (map (lambda: ([x : (Instance Card%)]) x) l))

(: install-state (State -> Void))
(define (install-state state)
  (send table begin-card-sequence)

  ;; erase all old snips
  (send table remove-cards draw-pile)
  (for ([stack (in-list stacks)])
    (send table remove-cards (stack-cards stack)))

  ;; restore old state
  (set! draw-pile (state-draw-pile state))
  (for ([stack (in-list stacks)]
        [cards (in-list (state-stacks state))])
    (set-stack-cards! stack cards))

  ;; restore GUI
  (for ([draw-pile-card (in-list draw-pile)])
    (send table add-card draw-pile-card 0 0))
  (send table move-cards-to-region draw-pile draw-pile-region)
  (for ([draw-pile-card (in-list (reverse draw-pile))])
    (send table card-face-down draw-pile-card)
    (send table card-to-front draw-pile-card))

  (for ([stack (in-list stacks)])
    (define num-cards (length (stack-cards stack)))
    (send table add-cards (stack-cards stack) 0 0)
    (send table move-cards (stack-cards stack)
          (stack-x stack)
          (stack-y stack)
          (lambda: ([i : Real])
            (values 0 (* (- num-cards i 1) card-space))))
    (send table cards-face-up (stack-cards stack))) 
  (send table end-card-sequence))

(: undo-stack (Listof state))
(define undo-stack null)

(: redo-stack (Listof state))
(define redo-stack null)

;; saves the current state in the undo stack
(define (save-undo)
  (set! undo-stack (cons (extract-current-state) undo-stack))
  (set! redo-stack null))

;; pre: (not (null? undo-stack))
(define (do-undo)
  (let ([to-install (car undo-stack)])
    (set! redo-stack (cons (extract-current-state) redo-stack))
    (set! undo-stack (cdr undo-stack))
    (install-state to-install)))

;; pre: (not (null? redo-stack))
(define (do-redo)
  (let ([to-install (car redo-stack)])
    (set! undo-stack (cons (extract-current-state) undo-stack))
    (set! redo-stack (cdr redo-stack))
    (install-state to-install)))

(define (position-cards [stack : stack])
  (let ([m (length (stack-cards stack))])
    (lambda ([i : Real])
      (values 0 (if (= m 0) 0 (* (- m i 1) card-space))))))

(define (reset-game)
  (send table remove-cards draw-pile)
  (for ([stack (in-list stacks)])
    (send table remove-cards (stack-cards stack)))

  (set! undo-stack null)
  (set! redo-stack null)

  (let* ([deck (shuffle-list (make-deck) 7)]
         [set-stack
          (lambda ([which : (All (A B) (Listof A) -> A)])
            (set-stack-cards! (which stacks) (list (which deck))))])
    (for ([card (in-list deck)])
      (send card user-can-move #f)
      (send card user-can-flip #f))
    (set! draw-pile (cddddr deck))
    (set-stack car)
    (set-stack (plambda: (A B) ([x : (Listof A)]) : A (cadr x)))
    (set-stack (plambda: (A B) ([x : (Listof A)]) : A (caddr x)))
    (set-stack (plambda: (A B) ([x : (Listof A)]) : A (cadddr x))))

  (for ([stack (in-list stacks)])
    (send table add-cards
          (stack-cards stack)
          (stack-x stack)
          (stack-y stack)
          (position-cards stack))
    (for ([card (in-list (stack-cards stack))]) (send card flip)))

  (send table add-cards-to-region draw-pile draw-pile-region))

(define (move-from-deck)
  (save-undo)
  (unless (null? draw-pile)
    (: move-one : (All (A B) (Listof A) -> A) -> Any)
    (define (move-one select)
      (let ([stack (select stacks)]
            [card (select draw-pile)])
        (set-stack-cards! stack (cons card (stack-cards stack)))
        (send table card-to-front card)
        (send table flip-card card)))

    (send table begin-card-sequence)
    (move-one car)
    (move-one (plambda: (A B) ([x : (Listof A)]) : A (cadr x)))
    (move-one (plambda: (A B) ([x : (Listof A)]) : A (caddr x)))
    (move-one (plambda: (A B) ([x : (Listof A)]) : A (cadddr x)))
    (send table end-card-sequence)

    (let ([cards-to-move (list (car draw-pile)
                               (cadr draw-pile)
                               (caddr draw-pile)
                               (cadddr draw-pile))])
      (send table move-cards cards-to-move
            0 0
            (lambda ([i : Integer])
              (define stack (list-ref stacks i))
              (define-values [dx dy] ((position-cards stack) 0))
              (values (+ dx (stack-x stack))
                      (+ dy (stack-y stack))))))

    (set! draw-pile (cddddr draw-pile))

    (send table move-cards-to-region draw-pile draw-pile-region)))

(: move-to-empty-spot ((Instance Card%) Stack -> Void))
(define (move-to-empty-spot card stack)
  (save-undo)
  (send table move-cards
        (list card)
        (stack-x stack)
        (stack-y stack)
        (position-cards stack))
  (remove-card-from-stacks card)
  (set-stack-cards! stack (cons card (stack-cards stack))))

(: remove-card ((Instance Card%) -> Void))
(define (remove-card card)
  (save-undo)
  (send table remove-card card)
  (remove-card-from-stacks card))

(: remove-card-from-stacks ((Instance Card%) -> Void))
(define (remove-card-from-stacks card)
  (define old-cards (map stack-cards stacks))
  (for ([stack (in-list stacks)])
    (set-stack-cards! stack (remove card (stack-cards stack))))
  (for ([stack     (in-list stacks)]
        [old-cards (in-list old-cards)])
    (unless (equal? (stack-cards stack) old-cards)
      (send table move-cards
            (stack-cards stack)
            (stack-x stack)
            (stack-y stack)
            (position-cards stack)))))

(send table set-single-click-action
      (lambda: ([card : (Instance Card%)])
        (if (send card face-down?)
          (move-from-deck)
          (let ([bottom-four
                 (let loop : (Listof (Instance Card%))
                   ([l : (Listof Stack) stacks])
                   (if (null? l)
                     null
                     (let ([stack (car l)])
                       (if (null? (stack-cards stack))
                         (loop (cdr l))
                         (cons (car (stack-cards stack)) (loop (cdr l)))))))])
            (when (member card bottom-four)
              (if (ormap (lambda: ([bottom-card : (Instance Card%)])
                           (and (eq? (send card get-suit)
                                     (send bottom-card get-suit))
                                (or (and (not (= 1 (send card get-value)))
                                         (= 1 (send bottom-card get-value)))
                                    (and (not (= 1 (send card get-value)))
                                         (< (send card get-value)
                                            (send bottom-card get-value))))))
                         bottom-four)
                (remove-card card)
                (let loop ([stacks stacks])
                  (if (null? stacks)
                    (void)
                    (let ([stack (car stacks)])
                      (if (null? (stack-cards stack))
                        (move-to-empty-spot card stack)
                        (loop (cdr stacks))))))))))
              (check-game-over)))

(define (game-over?)
  (and (null? draw-pile)
       (let ([suits/false
              (map (lambda: ([x : Stack])
                     (let ([stack-cards (stack-cards x)])
                       (if (null? stack-cards)
                         #f
                         (send (car stack-cards) get-suit))))
                   stacks)])
         (and (not (member #f suits/false))
              (memq 'clubs    suits/false)
              (memq 'diamonds suits/false)
              (memq 'hearts   suits/false)
              (memq 'spades   suits/false)))))

(define (won?)
  (and (game-over?)
       (andmap (lambda: ([x : Stack])
                 (define cards (stack-cards x))
                 (and (not (null? cards))
                      (null? (cdr cards))
                      (= 1 (send (car cards) get-value))))
               stacks)))

(define (check-game-over)
  (when (game-over?)
    (case (message-box "Aces"
                       (if (won?)
                         "Congratulations! You win! Play again?"
                         "Game Over. Play again?")
                       table
                       '(yes-no))
      [(yes) (reset-game)]
      [(no) (send table show #f)])))

(send table add-region draw-pile-region)
(reset-game)

(define mb (or (send table get-menu-bar)
               (make-object menu-bar% table)))

(define edit-menu (new menu% [parent mb] [label (string-constant edit-menu)]))
(new menu-item%
     [label (string-constant undo-menu-item)]
     [parent edit-menu]
     [callback (lambda (x y) (do-undo))]
     [shortcut #\z]
     [demand-callback
      (lambda (item) (send item enable (not (null? undo-stack))))])
(new menu-item%
     [label (string-constant redo-menu-item)]
     [parent edit-menu]
     [callback (lambda (x y) (do-redo))]
     [shortcut #\y]
     [demand-callback
      (lambda (item) (send item enable (not (null? redo-stack))))])

(send table show #t)
