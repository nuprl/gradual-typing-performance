#lang typed/racket

(require (prefix-in mred: typed/racket/gui))


(provide Region)

(define-type (Hash a b) (Boxof (Listof (Pairof a b))))

(provide Hash Mode Orientation Suit Card% Dir Table%)

(require/typed "region.rkt"
               [#:opaque Region region?])


(define-type Dir (U 'n 'e 's 'w))
(define-type Mode (U 'cw 'ccw 0 90 -90 180 -180 270 -270 360))
(define-type Orientation (U 0 90 180 270))
(define-type Suit (U 'clubs 'diamonds 'hearts 'spades 'unknown))
(define-type Card%
  (Class #:implements mred:Snip%
         (init [-suit-id Any]
               [-value Any]
               [-width Natural]
               [-height Natural]
               [-front (Instance mred:Bitmap%)]
               [-back (Instance mred:Bitmap%)]
               [-mk-dim-front (-> (Instance mred:Bitmap%))]
               [-mk-dim-back (-> (Instance mred:Bitmap%))]
               [-rotated-bms (Hash (Pairof Dir (Instance mred:Bitmap%)) (Instance mred:Bitmap%))])
         [card-width (-> Natural)]
         [card-height (-> Natural)]
         
         [copy (-> (Instance Card%))]
         
         [flip (-> Void)]
         [face-up (-> Void)]
         [face-down (-> Void)]
         [face-down? (-> Boolean)]
         [rotate (Mode -> Void)]
         [orientation (-> Orientation)]
         [get-suit-id (-> Any)]
         [get-suit (-> Suit)]
         [get-value (-> Any)]
         [semi-flip (-> Void)]
         [user-can-flip (case-> (-> Boolean)
                                (Any -> Void))]
         [user-can-move (case-> (-> Boolean)
                                (Any -> Void))]
         [snap-back-after-move (case-> (-> Boolean)
                                       (Any -> Void))]
         [stay-in-region (case-> (-> (Option Region))
                                 ((Option Region) -> Void))]
         [home-region (case-> (-> (Option Region))
                              ((Option Region) -> Void))]
         [dim (case-> (-> Boolean)
                      (Any -> Void))]
         [remember-location ((Instance mred:Editor<%>) -> Boolean)]
         [back-to-original-location ((Instance mred:Pasteboard%) -> Void)]))

(define-type Table%
    (Class #:implements mred:Frame%
           (init
            [title String]
            [w Real]
            [h Real]
            [parent (Option (Instance mred:Frame%)) #:optional]
            [width (Option Integer) #:optional]
            [height (Option Integer) #:optional]
            [x (Option Integer) #:optional]
            [y (Option Integer) #:optional]
            [enabled Any #:optional]
            [border Natural #:optional]
            [spacing Natural #:optional]
            [alignment (List (U 'left 'center 'right)
                             (U 'top 'center 'bottom))
                       #:optional]
            [min-width (Option Natural) #:optional]
            [min-height (Option Natural) #:optional]
            [stretchable-width Any #:optional]
            [stretchable-height Any #:optional])
           (augment [on-close (-> Void)])
           [table-width (-> Real)]
           [table-height (-> Real)]
           [begin-card-sequence (-> Void)]
           [end-card-sequence (-> Void)]
           [add-card ((Instance Card%) Real Real -> Any)]
           [add-cards (case-> ((Listof (Instance Card%)) Real Real -> Void)
                              ((Listof (Instance Card%)) Real Real (Any -> (Values Real Real)) -> Void))]
           [add-cards-to-region ((Listof (Instance Card%)) Region -> Void)]
           [move-card (-> (Instance Card%) Real Real Void)]
           [move-cards (case-> ((Listof (Instance Card%)) Real Real -> Void)
                               ((Listof (Instance Card%)) Real Real (Any -> (Values Real Real)) -> Void))]
           [move-cards-to-region (-> (Listof (Instance Card%)) Region Void)]
           [card-location ((Instance Card%) -> (Values Real Real))]
           [all-cards (-> (Listof (Instance mred:Snip%)))]
           [remove-card ((Instance Card%) -> Void)]
           [remove-cards ((Listof (Instance Card%)) -> Void)]
           [flip-card ((Instance Card%) -> Void)]
           [flip-cards ((Listof (Instance Card%)) -> Void)]
           [rotate-card ((Instance Card%) Mode -> Void)]
           [rotate-cards ((Listof (Instance Card%)) Mode -> Void)]
           [card-face-up ((Instance Card%) -> Void)]
           [cards-face-up ((Listof (Instance Card%)) -> Void)]
           [card-face-down ((Instance Card%) -> Void)]
           [cards-face-down ((Listof (Instance Card%)) -> Void)]
           [card-to-front ((Instance Card%) -> Void)]
           [card-to-back ((Instance Card%) -> Void)]
           [stack-cards ((Listof (Instance Card%)) -> Void)]
           [add-region (Region -> Void)]
           [remove-region (Region -> Void)]
           [hilite-region (Region -> Void)]
           [unhilite-region (Region -> Void)]
           [set-button-action ((U 'left 'middle 'right) Symbol -> Void)]
           [set-double-click-action ((-> (Instance mred:Snip%) Void) -> Void)]
           [set-single-click-action (-> (-> (Instance Card%) Void) Void)]
           [pause (-> Real Void)]
           [animated (case-> (-> Boolean)
                             (Any -> Void))]
           [set-status (-> String Void)]
           [create-status-pane (-> (Instance mred:Horizontal-Pane%))]
           [add-help-button (-> (Instance mred:Area-Container<%>) (Listof String) String Any (Instance mred:Button%))]
           [add-scribble-button (-> (Instance mred:Area-Container<%>) Module-Path String (Instance mred:Button%))] ; may need type for tag
           ))
