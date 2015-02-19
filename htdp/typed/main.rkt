#lang typed/racket/base

(require benchmark-util)

(require/typed/check "xhtml.rkt"
  [main (-> String Void)])

(time
  (with-output-to-file "/dev/null"
    (lambda ()
    (main "release"))
    ;(main "draft")
    ;(main "file.scrbl")
  #:exists 'append))
