suffixtree
==========

Danny Yoo's [suffixtree](https://github.com/dyoo/suffixtree) library.

This benchmarks implements longest-common-substring using Ukkonen's
suffix tree algorithm.

We test lcs on all pairs of lines in a file in `base/`:
- the small test is `hunt.txt`
- the large test is `prufock.txt`

---

This version of the benchmark defines untyped data & adds a typed wrapper.
Update functions are provided explicitly by this wrapper.

Gets a huge slowdown on the fully typed version. Beware!

