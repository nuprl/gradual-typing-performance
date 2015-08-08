#lang racket/base

;; Test unsafe require forms

(module a racket/base
  (struct foo (x y))
  (define a-foo (foo 1 2))
  (provide (struct-out foo) a-foo))

(module b typed/racket
  (require/typed racket/contract/combinator [#:opaque Blame exn:fail:contract:blame?])
  (require typed/racket/unsafe)
  (require/typed/unsafe (submod ".." a)
         [#:opaque Foo foo?] [foo (-> String String Foo)] [foo-x (-> Foo String)] [foo-y (-> Foo String)]
                        ;[#:struct foo ([x : String] [y : String])]
                        [a-foo Foo])

  (provide foo foo? foo-x foo-y)

  ;; UNSAFE
  ;; primitive error, no blame should be raised
  (with-handlers ([(negate exn:fail:contract:blame?) void])
    (string-append (foo-x a-foo))))

(module c typed/racket
  (require/typed racket/contract/combinator [#:opaque Blame exn:fail:contract:blame?])
  (require typed/racket/unsafe)
  (require/typed/unsafe racket/base
                        [string-append (-> String String Integer)])

  (require (submod ".." b))
  (foo-x (foo "yo" "lo"))

  ;; UNSAFE
  (with-handlers ([(negate exn:fail:contract:blame?) void])
    (number->string (string-append "foo" "bar"))))

(require 'b 'c)
