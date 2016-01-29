From John Clements.

Need a `provide/opaque`, or a way to instantiate the recursive contract
 on the untyped side.

```
#lang racket

(require pfds/trie)

(define (rand-list)
  (for/list ([i (in-range 128)])
      (random 256)))

(define t (trie (list (rand-list))))
(define u (time (bind (rand-list) 0 t)))
```
