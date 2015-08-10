#lang typed/racket/base


;(require/typed racket/contract/combinator [#:opaque Blame exn:fail:contract:blame?])
(require typed/racket/unsafe)
(require/typed/unsafe "a.rkt"
                      [#:struct foo ([x : String] [y : String])]
                      [a-foo foo])

;; UNSAFE
;; primitive error, no blame should be raised
;(with-handlers ([(negate exn:fail:contract:blame?) void])
;  (string-append (foo-x a-foo)))

(provide (struct-out foo))
