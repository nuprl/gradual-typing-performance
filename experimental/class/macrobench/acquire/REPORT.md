
Running time is 93.96% contracts
11604/12350 ms

(-> (instanceof/c (recursive-contract State%676707 #:impersonator)) tile? (or/c #f hotel?) (listof (cons/c player? (cons/c (listof (cons/c hotel? (cons/c (or/c #f #t) ()))) ()))) shares-order? (-> any/c tile?) (values any/c (instanceof/c (recursive-contract State%692711 #:impersonator)))) @ #(struct:srcloc /home/ben/code/racket/benchmark/gradual-typing-performance/class/macrobench/acquire/variation0001/tree.rkt 13 12 446 9)
  tree-next
  15907/2 ms


Running time is 13.36% contracts
150/1123 ms

(-> (recursive-contract anonymous-contract #:impersonator) tile? (or/c #f hotel?) (listof (cons/c player? (cons/c (listof (cons/c hotel? (cons/c (or/c #f #t) ()))) ()))) shares-order? (-> any/c tile?) (values any/c (recursive-contract anonymous-contract #:impersonator))) @ #(struct:srcloc /home/ben/code/racket/benchmark/gradual-typing-performance/class/macrobench/acquire/variation0001/tree.rkt 13 12 446 9)
  tree-next
  113/2 ms
