#lang typed/racket

(require "data.rkt")
(require/typed
    2htdp/image
  [#:opaque Image image?]
  [overlay   (-> Image Image Image)]
  [circle    (-> Integer String String Image)]
  [rectangle (-> Integer Integer Color Color Image)]
  [place-image (-> Image Integer Integer Image Image)]
  [empty-scene (-> Integer Integer Image)])

(provide   
 Image
 overlay
 circle
 rectangle
 place-image
 empty-scene)
