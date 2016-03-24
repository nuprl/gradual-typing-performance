jfp
===

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
