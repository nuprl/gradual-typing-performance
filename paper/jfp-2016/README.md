jfp
===

quarantine the refs. to "typed racket"
run-time vs runtime vs run time
typecheck vs type-check vs type check
vary inputs, confirm the same lattice
benchmark vs program


---

Argument: our gradual typing is THE BEST gradual typing
It's the only coherent one -- that is the meta-point of this journal paper.


--- 

It's not about paths. It's a random walk.
You can't do a path in i.e. dr racket, with 20,000 (?) modules
- "no paths if we do not tolerate overhead along the path"
- "never start at bottom & go to top, only make a few steps, different scenario"
- just academic question to try & find a best path

"Not all paths are equally likely"
WE DONT KNOW. We really don't know! Really!


---
- What is to be done?
- come to bury sound gradual typing
- adaptors vs. "DefinitelyTyped" d.ts
- experience report, lessons
- prediction -- we tried something, works pretty bad, take what you will
- 

Matthias
---

1. There is GT design space, we have framework, "extended abstract"
2. What is GT
   Give scholarly credit where due, citations only
   Micro vs. Macro
3. Need evaluation, nobody has done that
4. Our story, how GT should be used
   The other guys don't have a story
   "benefits of comining static + dynamic"
5. As before, more or less

Memo
---
- experimental protocol (stop early)
- adaptor
- conversion lessons / usability
- 

Building
---
- Change `scribble/jfp` to look for `jfp.cls` instead of `jfp1.cls`
- Download `ilfonts`, then `\usepackage{ilfonts}`

---

 @;dynamic features@~cite[tsth-esop-2013 vksb-dls-2014 sfrbcsb-popl-2014] in gradual type systems,
 @;continuation-passing@~cite[tsth-esop-2013],
 @;object identity checks@~cite[vksb-dls-2014], and
 @;run-time evaluation@~cite[sfrbcsb-popl-2014],

Building
---
- Change `scribble/jfp` to look for `jfp.cls` instead of `jfp1.cls`
- Download `ilfonts`, then `\usepackage{ilfonts}`


Not used
- `tlmgr --update self`
- `tlmbr install ec`
