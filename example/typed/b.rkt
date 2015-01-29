#lang typed/racket

(require benchmark-util)

(require/typed/check "a.rkt" [f (-> Integer Integer)])

(for ([i 100]) (f i))
