
(module snipclass typed/racket
  (require typed/racket/gui)
  (provide sc)
  (define sc (make-object snip-class%))
  (send sc set-classname "card")
  (send (get-the-snip-class-list) add sc))

