stack
===

Problem: untyped data is always untrusted & checked,
 even when the data was originally from a typed source.

This benchmark has 2 modules:
- stack.rkt : define a very simple data structure
- main.rkt  : ask stack.rkt for a large data structure,
              do lots of stack.rkt operations

