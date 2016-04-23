suffixtree
==========

Danny Yoo's [suffixtree](https://github.com/dyoo/suffixtree) library.

This benchmarks implements longest-common-substring using Ukkonen's
suffix tree algorithm.

We test lcs on all pairs of lines in a file in `base/`:
- the small test is `hunt.txt`
- the large test is `prufock.txt`

---

This version uses an adapter module.
So, typed files can use typed data and untyped files can use untyped data.

base files
----------
- `hunt.txt` small text file, for running LCS on lines
- `prufock.txt` larger text file, same purpose
- `code-sample.rkt` source for the untyped suffixtree, run LCS on itself
