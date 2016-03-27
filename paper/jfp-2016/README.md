jfp
===

- GCTIME
  - Emery : https://people.cs.umass.edu/~emery/pubs/gcvsmalloc.pdf
    - 19% with 3x heap
    - 70% with smaller heap
  - ???   : https://people.cs.umass.edu/~immerman/pub/hertz.pdf
    - 20% "acceptable for personal computers
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

---


quarantine the refs. to "typed racket"
run-time vs runtime vs run time
typecheck vs type-check vs type check
vary inputs, confirm the same lattice
benchmark vs program
no rhetorical questions


--- 

It's not about paths. It's a random walk.
You can't do a path in i.e. dr racket, with 20,000 (?) modules
- "no paths if we do not tolerate overhead along the path"
- "never start at bottom & go to top, only make a few steps, different scenario"
- just academic question to try & find a best path

"Not all paths are equally likely"
WE DONT KNOW. We really don't know! Really!



Building
---
- Change `scribble/jfp` to look for `jfp.cls` instead of `jfp1.cls`
- Download `ilfonts`, then `\usepackage{ilfonts}`

---

 @;dynamic features@~cite[tsth-esop-2013 vksb-dls-2014 sfrbcsb-popl-2014] in gradual type systems,
 @;continuation-passing@~cite[tsth-esop-2013],
 @;object identity checks@~cite[vksb-dls-2014], and
 @;run-time evaluation@~cite[sfrbcsb-popl-2014],
