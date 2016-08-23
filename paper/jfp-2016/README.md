jfp
===

- text around the scatters
  1. motivate fine-grained comparisons
  2. what it is, scatterplots
     what we learn from them
  3. cannot scale plots but
     CAN scale the lessons, add table
  (first need to experiment)
- for big ones, report where 95%ci overlaps?
- compile times ... faster!


M: every paragraph needs the same template:
- thesis
  - support
  - support
  - support

sometimes you can end with 'however' and do:
- antithesis
  - support
  - support
  - support

---

TR users / performance complaints
 now where to put these? GTP website? appendix?

- marc burns (2x overhead)
  http://con.racket-lang.org/2015/burns.pdf
- Konrad Hinsen (frustrated, typed functions don't match untyped)
  M.B. also involved and frustrated
  https://groups.google.com/forum/#!searchin/racket-users/konrad$20hinsen|sort:relevance/racket-users/-RI1p1Z1ZRE/JFB1QoPiKdcJ
- Dan Praeger (learn by doing)
  https://groups.google.com/forum/#!searchin/racket-users/konrad$20hinsen|sort:relevance/racket-users/2XjtfLS2wAY/A3ESNSnIBAAJ
- John Carmack (exists, happy)
  https://groups.google.com/forum/#!searchin/racket-users/konrad$20hinsen|sort:relevance/racket-users/RFlh0o6l3Ls/G7Aceod-IX8J
- Nota Poin (what is the overhead?)
  https://groups.google.com/forum/#!searchin/racket-users/typed$20racket|sort:relevance/racket-users/7tqsn7RnY6o/TYNQwTxhAwAJ
- WarGrey (TR for web server?)
  https://groups.google.com/forum/#!searchin/racket-users/typed$20racket|sort:relevance/racket-users/OZWWXMWcMC4/7kUdDa_hYVwJ
- Michael B + Neil T. (performance bottlenecks)
  https://groups.google.com/forum/#!searchin/racket-users/typed$20racket|sort:relevance/racket-users/oo_FQqGVdcI/Y9PXwxB7He8J
- JCG (50% performance hybrid)
  https://groups.google.com/forum/#!searchin/racket-users/performance$20hybrid%7Csort:relevance/racket-users/rfM6koVbOS8/PYRBvv5mBwAJ

---

- GCTIME
  - Emery : https://people.cs.umass.edu/~emery/pubs/gcvsmalloc.pdf
    - 19% with 3x heap
    - 70% with smaller heap
  - ???   : https://people.cs.umass.edu/~immerman/pub/hertz.pdf
    - 20% "acceptable for personal computers"
  - Ellis, Appel, Li : http://www.hpl.hp.com/techreports/Compaq-DEC/SRC-RR-25.pdf
    - 
  - ???   : /Users/ben/Downloads/mmtk-sigmetrics-2004.pdf 
    - 14% is high
    - 
  - ???   : http://www.cs.utexas.edu/users/mckinley/papers/mmtk-icse-2004.pdf
  - Ungar https://people.cs.umass.edu/~emery/classes/cmpsci691s-fall2004/papers/p157-ungar.pdf

- use confidence intervals when reporting measn
  https://www.cs.kent.ac.uk/pubs/2012/3233/
  (don't be a statistic in future statistics papers)
- better example in intro
- fix acquire, put tree boundary back
- lattice clearly leaves questions unanswered
  (which modules to type next?)
  (cost/boundary)


Building
---
- Change `scribble/jfp` to look for `jfp.cls` instead of `jfp1.cls`
- Download `ilfonts`, then `\usepackage{ilfonts}`

---
