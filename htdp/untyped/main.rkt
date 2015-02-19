#lang racket/base

(require "xhtml.rkt")

(time
  (with-output-to-file "/dev/null"
    (lambda ()
    (main "release"))
    ;(main "draft"))
    ;(main "file.scrbl")
  #:exists 'append))
