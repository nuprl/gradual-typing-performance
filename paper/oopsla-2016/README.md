oopsla-2016
===
(maybe oopsla)


TELL A STORY.
There is a tradeoff between types for software engineering and performance.

Typed Racket so far has demanded type soundness, but offered type-driven optimization.
Sometimes, the optimizations speed up the program.
More often, the optimizations are totally outclassed by the cost of run-time soundness checks.

What if we could turn off the soundness? Type-check, but erase to untyped Racket.
Then we could recover performance.

But the cost is that we can no longer trust the types.


Example
---
Consider this common practice in untyped code:

```
   ;; Start with an untyped program
   (define (f x)
     (assert (<= 0 x))
     (if (< x 2) 1 (* x (f (- x 1)))))

   ;; ===>

   ;; Convert to a contract-protected program,
   ;;  remove the assertion
   (define/contract (f+ x) (-> natural natural)
     (if (< x 2) 1 (* x (f+ (- x 1)))))
```

One might hope the same transformation works in Typed Racket, namely:

```
    (: f++ (-> Natural Natural))
    (define (f++ x)
      (if (zero? x) 1 (* x (f++ (- x 1)))))
```

And it does, but not if the type is unsound.
In that case you would get an infinite loop.
