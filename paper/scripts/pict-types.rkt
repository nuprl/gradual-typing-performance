#lang typed/racket/base

(require/typed/provide pict
  [#:opaque Pict pict?]
  [text (-> String (U String (cons Symbol String)) Natural Pict)]
  [vc-append (-> Real Pict Pict Pict)]
  [vl-append (-> Real Pict * Pict)]
  [vr-append (-> Real Pict * Pict)]
  [hc-append (-> Real Pict * Pict)]
  [ht-append (-> Real Pict * Pict)]
  [blank (-> Real Real Pict)]
  [pict-height (-> Pict Real)])

