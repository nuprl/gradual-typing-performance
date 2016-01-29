#lang racket

(require 2htdp/universe)

;; FIXME: no-check mode to avoid TR submodule bug
(module callbacks typed/racket/no-check
  (require typed/2htdp/image)
  (provide draw stop)

  (: draw (-> Integer Image))
  (define (draw world)
    (place-image (circle 30 "solid" "blue")
                 100
                 world
                 (empty-scene 200 300)))

  (: stop (-> Integer Boolean))
  (define (stop world)
    (< world 0)))

(require (submod "." callbacks))

(big-bang 300
          (on-tick sub1)
          (to-draw draw)
          (stop-when stop))
