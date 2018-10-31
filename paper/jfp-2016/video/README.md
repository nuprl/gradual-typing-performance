video
===

JFP accepts (up to) 10-minute videos with accepted papers.

Lets make a video for this paper.

Current plan (2018-10-22): make slides, do a voice-over.


Outline
---

- Title, authors, history of authors
- Purpose: a method to evaluation the perf. of a gradual typing system
- What does it mean for _typing system_ to have performance?
- Well, GT is about mixing
- Types are claims
- Mixing means claims might be out of sync
- Cost to enforce claims at the boundary
- Anecdotal evidence, can have a BIG impact
- Emails from users
- Method 1: space = lattice
- Method 2: D-deliverable metric
- Method 3: plots (D-deliv => lots of lattices ; lattice => bar ; histogram => lots of samples)
- Summary of method, see paper for application to TR
- THREATS TO VALIDITY
- serious threat, does not scale ... micro gt
- Q. can we approximate the % of D-deliverable configs out of 2^N from O(N) measures?
- yes (for details see cost of type-tag soundness ?)
- imagine a big lattice , fix value for D ... D=4
- pick 1 at random, fill in 1 blank, get a proportion
- at limit, get true proportion
- before then, more is better
- fix S take R samples ... should all be close lets combine
- Protocol: S,R; R-%D's; 95-ci
- instantiate S=10N R=10 apply to big benchmarks get sleeve of truth
- judge for yourself whether the approximations are good, but we conclude
- THREATS again, only works for this %-D-deliv question (but that's all we really care about)
  if you have a different metric that you think a programmer cares about, need to propose a method
  ... we can definitely think of systems-level metrics, but not user-level metrics
- in conclusion: problem, exhaustive, approximate
- in the paper: details, especially the TR application
- keep measuing in future

- special thanks to Sam TH


Script
---

(introduce self)
3-year team effort

program = collection of modules

type boundary

if we want to have a relation between types and
values, need to check (aka soundness)

if 4 components explore all 2^4 possible
programmer may start at bottom move up to some
 middle point do not know what must do all

communicate to language implementor

miserable

D = worst thing implementor willing to impose,
summarize entire lattice as a %
dont know what D works in practice --- dataset for more

This was Ben Greenman explaining our paper to you.
Thanks for listening and please leave comments on the
Journal’s web site. If you have questions, leave them
there too, and I will answer them in public
