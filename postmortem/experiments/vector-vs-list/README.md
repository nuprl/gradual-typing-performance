Vectors vs. Lists
=================

Accounting for vectors & lists

#### Goal:
- Add continuation mark when checking a vector or a list
  (see contract-continuation-mark-key or whatever in the repo)
- Profile these marks just like for regular contracts
- Check out the results


#### Notes:

- `vector.rkt` shows that vectors are blamed (and don't do recursive checks)
- `list.rkt` is shows the bad thing, that the list isn't directly blamed for the checks it causes

- `tetris-with-lists` original tetris, for comparison
- `tetris-with-vectors` tetris using vectors instead of lists,
   expected better contract reporting and instead got MUCH better performance.
   LIST = 30924ms
   VECTOR = 150ms

- `snake-with-lists` original snake
- `snake-with-vectors` vectorized snake
   expected better performance. Got much WORSE performance but fewer contracts
   LIST = 19936/20937 ms
   VECTOR = 5242/252137 ms
