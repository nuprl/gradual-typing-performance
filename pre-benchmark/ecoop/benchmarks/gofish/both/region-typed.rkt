#lang typed/racket

(require "typed-base.rkt"
         (prefix-in mred: typed/racket/gui))

(provide region-x region-y region-w region-h set-region-hilite?!
         region-paint-callback region-label region-button? region-hilite?
         region-callback region-decided-start? region-can-select?
         set-region-decided-start?! set-region-can-select?!
         region-interactive-callback make-region set-region-callback!)

(require/typed "region.rkt"
               [make-region (->* (Real Real Real Real (Option String))
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
               [region-callback (Region -> (Option (case-> (-> Any) ((Listof (Instance Card%)) -> Any))))]
               [region-decided-start? (Region -> Boolean)]
               [region-can-select? (Region -> Boolean)]
               [set-region-decided-start?! (Region Boolean -> Void)]
               [set-region-can-select?! (Region Boolean -> Void)]
               [region-interactive-callback (Region -> (Option (Boolean (Listof (Instance mred:Snip%)) -> Any)))])
