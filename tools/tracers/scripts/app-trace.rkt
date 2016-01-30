#lang racket/base

;; Proposal for the benchmark-util library
;; - Add a (provide/trace id ...) form to
;;    register `id` into a set.
;; - Override #%app to expand into a print&apply.
;;    Print which identifiers from the global set are being accessed.
