jfp
===

Section 2 outline
---
- typed racket has users
- users complain about slowdowns
- slowdowns come from boundaries
  - type -> contract, type boundary
  - potentially very slow (?example?)
  - soundness matters, moral turpitude

- subset of modules are typed
  - no idea which subset, maybe critical, maybe conventient
  - some modules impossible to type
    (shrinking, but ti's a thing)
  - some types impossible to boundary
    (ditto, shrinking)
  - i.e. not feasible to type the entire program
         nor even desirable, if feasible
  - but maybe can type a few more, especially if we know where to focus efforts
- when dev reports that a program is slow, usually has perf. target in mind
  - most common case is relative to untyyped
    "I change these baxk to Racket and get XXXx faster"
- stepping back, a performant GT system should be able to solve these perf.
  issues for any developer
  (Sec3 or Sec4?)
  - can't assume where they are
  - can't assume they can reach a few good points
  - but proportions might just make sense
- suffixtree, to make ideas concrete

- lattice clearly leaves questions unanswered
  (which modules to type next?)
  (cost/boundary)

- this is hard question (we don't have answer)
  - articulate challenges of answering
  1. boundaries not clear
    - 
  2. cost/boundary not clear
    - polymorphic vs. large list vs. frequent calls
  3. many boundaries
    - 
  4. a
- in practice, have types where essential / convenient
  - never convert the whole system

HMPH really just eant to say that developers are lost in the lattice

Reseach agenda

- (why did we do that?)
- having articulated 

(Sec 3)
- developer goals => framework



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

