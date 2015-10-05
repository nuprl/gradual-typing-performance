suffixtree
==========

Danny Yoo's [suffixtree](https://github.com/dyoo/suffixtree) library.

This benchmarks implements longest-common-substring using Ukkonen's
suffix tree algorithm.

We test lcs on all pairs of lines in a file in `base/`:
- the small test is `hunt.txt`
- the large test is `prufock.txt`

Notes
-----
- Added adaptor for all structs in `data.rkt`.
