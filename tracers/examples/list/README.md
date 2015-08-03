list
====

Question: when I check a contract on a list, do I get:
- One `O(n)` List contract check
- A series of `O(n)` Element contract checks (and nothing for the overall list)

The point is, we need to know if each "count" accumulated by tracing has about
the same cost.

results
-------

The checks are only flat ones: we count `O(n)` checks for a list.
Strangely, the checks for a list of functions are NOT accounted.

(The checks for `require/typed` functions seem just fine though.)
