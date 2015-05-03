#lang racket/base

(provide (struct-out stream))

(struct stream (first rest))

