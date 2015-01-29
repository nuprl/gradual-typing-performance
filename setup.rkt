#! /usr/bin/env racket
#lang racket

;; `./setup.rkt <project>` will do everything you need to set up a
;; project for running benchmarks, assuming that you have the
;; right directory structure.

(require racket/cmdline)

(command-line
 #:args (path)
 (let ()
   (system "raco pkg install --skip-installed tools/benchmark-util/")
   (dynamic-require '(submod "tools/setup-benchmark.rkt" main) #f)
   (void)))
