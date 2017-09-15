2016-05-17
===

Notes for a principal investigators' meeting talk.

20 minutes.
"On the Performance Bottleneck of Gradual Typing, especially Typed Racket"

DANGER
---
take5-transient somehow has 2 extra data points. To fix I deleted two at random.

High-level outline
---
- show TR data
- show reticulated data
- get feedback on data / methodology


TODO
---
- [ ] 
- [ ] retic L-NM
- [ ] 
- [ ] 
- [ ] 


Check-in 2016-05-12
---
My goal: show data we collected, generate questions, point out bottlenecks
         also show reticulated
         also 3/10 is not usable
worry: do NOT want to start with L-NM figures
       too far removed from the real problems
think: give general intro to gradual typing via our website
       segue into known performance problems
       state "the important" questions --- parameters
       segue from USER to IMPLEMENTOR
         (gradual typing promise!)
       challenges, LNM plot (for website?)
       using LNM to compare
       any questions on purpose of these figures?
         really is tool to see lattice


Low-level outline
---

website not performance critical,
 but many TR users have followed similar path and gotten bad results

OH HANG ON last few seconds I want to talk about reprod.
  been running similar experiment on reticulated


Questions
---
dead?
  we did a comprehensive performance evaluation
  had reason to believe TR would do well (most performant implementation of sound gt)
  resuts far worse than expected
  working hypothesis: exists an order-of-magnitude migration path
   totally false b/c tight coupling
  dead = not just a poor implementatino but 'insurmountable' difficulties
  IMO obvious, ya can't just mix types anywhere

