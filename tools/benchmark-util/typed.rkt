#lang typed/racket/base

;; Typed interface to `count-chap-utils.rkt`.
;;
;; Usage:
;;   (require benchmark-util/typed)
;; It's just the same as inlining this `require/typed` in your own module.

(require/typed/provide "count-chap-utils.rkt"
  (*count-chaps-out* (Parameterof String))
  (count-chaps (-> Void)))
