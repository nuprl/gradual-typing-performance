#lang typed/racket/base

(require benchmark-util)
(require/typed/check "streams-struct.rkt"
  [#:struct stream ([first : Natural] [rest : (-> stream)])])

(provide (struct-out stream))
