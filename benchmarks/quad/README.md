quad
====

Benchmark renders a document from the source code `quick-sample.rkt` using
Matthew Butterick's experimental [quad](http://github.com/mbutterick/quad) typesetter.

The `base/` folder has alternate versions of `quick-sample.rkt`.
To run the benchmark on these alternates, replace `quick-sample.rkt` with one of them.


Notes
-----
- Created adaptors for `ocm` and `penalty` structures.


History
-------
- Original author added types because his untyped program was too slow.
  Sadly the fully-typed version ran about 10x slower.
