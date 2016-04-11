higher-order
===

Check higher-order use of traced identifiers.

Should this even count as a use of `TRACED`?
```
  (define (f x) 0)
  (f TRACED)
```

It doesn't count for vector chaperones
