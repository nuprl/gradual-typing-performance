(module image typed/racket
  (require/typed
   2htdp/image
   [#:opaque Image image?]
   [circle (-> Real String String Image)]
   [empty-scene (-> Real Real Image)]
   [place-image (-> Image Real Real Image Image)])
  (provide
   Image
   circle
   empty-scene
   place-image)
  
  #;(define (image? x) •))
