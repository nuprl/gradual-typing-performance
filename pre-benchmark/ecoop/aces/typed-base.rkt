#lang typed/racket
(require (prefix-in mred: typed/racket/gui)
         "list-hash.rkt")
(provide Mode Orientation Suit Card% Dir)
;(require "region.rkt")
#;(provide region? region-x region-y region-w region-h set-region-hilite?!
         region-paint-callback region-label region-button? region-hilite?
         region-callback region-decided-start? region-can-select?
         set-region-decided-start?! set-region-can-select?!
         region-interactive-callback)

(require/typed/provide "region.rkt"
                       [#:opaque Region region?]
                       [make-region (->* (Real Real Nonnegative-Real Nonnegative-Real (Option String))
                                         ((Option (-> (Listof (Instance Card%)) Any)))
                                         Region)]
                       [set-region-callback! (-> Region (Option (-> (Listof (Instance Card%)) Any)) Void)]
                                            
                       [region-x (Region -> Real)]
                       [region-y (Region -> Real)]
                       [region-w (Region -> Nonnegative-Real)]
                       [region-h (Region -> Nonnegative-Real)]
                       [set-region-hilite?! (Region Boolean -> Void)]
                       [region-paint-callback (Region -> (Option ((Instance mred:DC<%>) Real Real Real Real -> Any)))]
                       [region-label (Region -> (Option (U String (Instance mred:Bitmap%))))] ; No idea if this is correct or not?
                       [region-button? (Region -> Boolean)]
                       [region-hilite? (Region -> Boolean)]
                       [region-callback (Region -> (Option (case-> (-> Any) ((Listof (Instance mred:Snip%)) -> Any))))] ; fix this should be interface ; can I make this one use Snip% ???
                       [region-decided-start? (Region -> Boolean)]
                       [region-can-select? (Region -> Boolean)]
                       [set-region-decided-start?! (Region Boolean -> Void)]
                       [set-region-can-select?! (Region Boolean -> Void)]
                       [region-interactive-callback (Region -> (Option (Boolean (Listof (Instance mred:Snip%)) -> Any)))])


(define-type Dir (U 'n 'e 's 'w))
(define-type Mode (U 'cw 'ccw 0 90 -90 180 -180 270 -270 360))
(define-type Orientation (U 0 90 180 270))
(define-type Suit (U 'clubs 'diamonds 'hearts 'spades 'unknown))
(define-type Card%
  (Class #:implements mred:Snip%
         (init [-suit-id Real]
               [-value Real]
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
         [get-suit-id (-> Real)]
         [get-suit (-> Suit)]
         [get-value (-> Real)]
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