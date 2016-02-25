jfp
===

monotone general -> specific (Sec 4 is anomoly)
state boundary / dynamic cost early
do NOT conflate gradual vs. optional typing

quarantine the refs. to "typed racket"
run-time vs runtime vs run time
typecheck vs type-check vs type check
vary inputs, confirm the same lattice
benchmark vs program
no rhetorical questions


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


Outline (NOPE)
---

* Part I: Evaluating GT
  - GT promise
  - Benfits of Soundness, Pitfalls of unsound types
  - Related work, industry projects
  - Evaluation framework
    . Developer's perspective
    . Sample lattice, L-N/M

* Part II: Case study, Typed Racket
  - Benchmark programs
    . quad BG, not MB
    . module graphs (try coloring again)
    . experience report, adaptor modules
  - L-N/M
    . all graphs (6.2-4), how to read
    . 6.2 discussion
    . vs 6.3/6.4 discussion; what changed?
  - What can be done?
    . benchmarks in depth, specific pathologies
    . are we dead?
  - Threats to validity
    . choice of types
    . choice of modularization
    . "small" benchmarks
    . changing implementation
    . "10x is not so bad"
  - Porting lessons (for developers)

* Part III: Conclusions & Future
  - Summarize L-N/M, easy improvements
  - Future tool support
    . profile untyped, to get boundary-crossing frequency
    . contract-profile when things are slow
    . (may want to change boundaries, to get performance without typing a whole file)
  - Scale to micro GT
  - 3 ways: tools, implementation, language design


Building
---
- Change `scribble/jfp` to look for `jfp.cls` instead of `jfp1.cls`
- Download `ilfonts`, then `\usepackage{ilfonts}`


Not used
- `tlmgr --update self`
- `tlmbr install ec`
