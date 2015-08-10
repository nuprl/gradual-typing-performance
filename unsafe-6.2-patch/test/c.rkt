#lang typed/racket

(require/typed racket/contract/combinator [#:opaque Blame exn:fail:contract:blame?])
(require typed/racket/unsafe)
(require/typed/unsafe racket/base
                      [string-append (-> String String Integer)])

;; UNSAFE
(with-handlers ([(negate exn:fail:contract:blame?) void])
  (number->string (string-append "foo" "bar")))
