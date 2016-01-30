#lang racket/base
(require racket/system)

;; Visually inspect these commands to see if plotting works

(module+ main

  (define-syntax-rule (interactive-test* [cmd descr] ...)
    (begin
      (begin (displayln descr)
             (system cmd)
             (system "firefox output.png")
             (displayln "Press enter to continue")
             (read-line)) ...))

  (interactive-test*
   ["raco gtp-lnm test/echo-data.rktd"
    "Standard Plot (echo)"]
   ["raco gtp-lnm -L '(0 1 2)'         test/suffixtree-data.rktd"
    "L=(0,1,2)"]
   ["raco gtp-lnm -L '(0 1 3)'         test/suffixtree-data.rktd"
    "L=(0 1 3)"]
   ["raco gtp-lnm -L '(0 1 2)' --split test/suffixtree-data.rktd"
    "Suffixtree, L=(0 1 2)"]
   ["raco gtp-lnm                      test/suffixtree-data.rktd test/echo-data.rktd"
    "Suffixtree + Echo"]
   ["raco gtp-lnm --aggregate          test/suffixtree-data.rktd test/echo-data.rktd"
    "Suffixtree + Echo (still 2 plots)"]
   ["raco gtp-lnm --aggregate test/suffixtree-2.rktd test/suffixtree-data.rktd"
    "Suffixtree + Echo, 1 plot"]
   ["raco gtp-lnm -L '(0 2)' -N 2 test/suffixtree-2.rktd test/suffixtree-data.rktd"
    "2 plots, 2 lines each"]
   ["raco gtp-lnm --split -L '(0 2)' -N 2 test/suffixtree-2.rktd test/suffixtree-data.rktd"
    "4 plots"]
   ["raco gtp-lnm -N 6 -M 15 -cutoff 0.3 -L '(0 2)' -N 2 test/suffixtree-data.rktd"
    "N,M,L,cutoff lines"]
   ["raco gtp-lnm --max-overhead 150 test/suffixtree-data.rktd"
    " LARGE max-overhead"]
   ["raco gtp-lnm --hist --max-overhead 150 test/suffixtree-data.rktd"
    " histogram, very fine-grained"]
   ["raco gtp-lnm --hist --num-samples 20 --max-overhead 100 test/suffixtree-data.rktd"
    "sparse histogram"]
   ["raco gtp-lnm -p -L '(0 1 2)' --split test/suffixtree-data.rktd"
    "paths, very boring"]
  )
)
