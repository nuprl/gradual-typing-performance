quad
====

Benchmark renders a document from the source code `quick-sample.rkt` using
Matthew Butterick's experimental [quad](http://github.com/mbutterick/quad) typesetter.

The `base/` folder has a few alternate versions of `quick-sample.rkt`.
To run the benchmark on these alternates, replace `quick-sample.rkt` with one of them.


Notes
-----
- Created adaptors for `ocm` and `penalty` structures.
- Original author added types because his untyped program was too slow.
  Unfortunately the fully-typed version was ~10x slower.
