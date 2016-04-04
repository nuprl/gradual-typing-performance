#lang typed/racket/base

(require benchmark-util)

(require/typed/check "image.rkt"
  (#:struct image ((impl : Any)))
  (empty-scene (-> Real Real Image))
  (place-image (-> Image Real Real Image Image))
  (circle (-> Real String String Image))
)
(define-type Image image)

(provide
  Image
  empty-scene
  place-image
  circle
)
