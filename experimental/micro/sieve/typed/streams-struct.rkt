#lang typed/racket/base

(provide (struct-out stream))

(struct stream ([first : Natural] [rest : (-> stream)]))

