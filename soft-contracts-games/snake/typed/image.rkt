(module image racket
  (require 2htdp/image)
  (define image/c (λ (x) (image? x)))
  (provide
   circle 
   empty-scene
   place-image)
  
  #;(define (image? x) •))
