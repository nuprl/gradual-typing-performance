#lang typed/racket/base

(require/typed/provide pict
  [#:opaque Pict pict?]
  [frame (->* [Pict] [#:color String] Pict)]
  [text (-> String Any Natural Pict)]
  [vc-append (-> Real Pict * Pict)]
  [vl-append (-> Real Pict * Pict)]
  [vr-append (-> Real Pict * Pict)]
  [hc-append (-> Real Pict * Pict)]
  [ht-append (-> Real Pict * Pict)]
  [blank (-> Real Real Pict)]
  [colorize (-> Pict String Pict)]
  [pict-height (-> Pict Real)])
