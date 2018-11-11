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

Hello, this is Ben Greenman speaking and I'm here to tell you about our team's
recent work.
Over the past few years we have developed and evaluated a method to measure the
performance of a gradual typing system.
The method is a tool for language implementors to understand the performance
implications of a design choice and to systematically compare different
implementations of gradual typing for the same underlying language.

To begin, lets see why performance is an issue for gradual typing.
In any programming language we can picture a program as a collection of
components.
Some components may depend on one another, and at runtime value may cross these
dependency edges.
In a gradually typed language, a developer is free to make some components
statically typed and other components dynamically typed.
Static and dynamic components may depend on one another, and in this case we
call the edge between them a type boundary.

A type boundary is a channel of communication.
For example, a typed component may request an integer and receive the untyped
value 42.
In this case the type and value happen to match.
In general, if we care about maintaining a relationship between static types
and runtime values, then we need to watch for possible mis-matches.
For example, a typed component may ask for an Integer and receive the untyped
value NAN.
In this case we need a check to detect the mismatch.
 
These safety checks impose a runtime cost.
The size of the cost depends on the interaction.
If a typed component expects a list, it may need to traverse the given value to
identify a mismatch.
If typed code expects a function, it may choose to wrap the given value in a
proxy that monitors its behavior.
The bottom line is that type boundaries lead to a non-trivial runtime cost.
And so, for a program we would like to understand its performance and for a
gradual typing system we would like to understand its performance implications.
For this, we need a systematic method.

Our method starts with the acknowledgment that in a program with N components a
developer has 2 to the N possibilities to choose from.
One could begin fully untyped, add annotations to a few modules, and wind up
somewhere in the middle of the space.
We have no data to suggest that developers will avoid certain configurations or
gravitate towards others, so the method considers all possibilities.

Each gradually typed configuration represents a complete program, so the next
step is to measure their running times.
Now we have an exponentially large dataset and the challenge is to find a
compact summary.
First, choose a baseline.
The untyped configuration is one possible choice.
Second, compute the overhead of each configuration relative to the baseline.
From these normalized numbers it is clear that some configurations have decent
performance and others are bad.
And so we condense the huge set of running times into one ration.
More precisely, we introduce a parameterized measure: a configuration is
D-deliverable for some value of D if its running time is no worse than D times
the baseline.
The idea is that a language implementor can pick a value for D that represents
the worst overhead they are willing to impose on users.

In summary, start with any program, apply gradual typing, explore all
possibilities, and count the proportion of D-deliverable configurations for
your chosen value of D.

We have also developed a method for plotting the results for a general audience
on a two-dimensional axis.
On the x-axis, we have values of D from 1 to some large upper bound.
On the y-axis we have the percent of D-deliverable configurations.
In the paper we choose 20x as our upper bound.
For any program, we can now see how increasing the acceptable overhead along
the x-axis increases the proportion of D-deliverable configurations.
The line on such a plot should ideally reach the top of the y-axis at a low
value of D.
But as a general rule, a larger area under the curve is better.

One limitation of our method is that it requires an exponential number of
measurements.
A program with N components implies 2 to the N configurations.
To avoid this explosion, we developed a method based on simple random sampling.
The idea is to sample a linear number of configurations, then count the
proportion of D-deliverable configurations in the same.
Repeating this process for a few samples leads to a statistically significant
approximation of the true proportion of D-deliverable configuraitons.
For details and an empirical justification, see the paper.

You can also read about:
 an application of the original, exhaustive method to Typed Racket on a suite
 of 20 benchmark programs, a direct comparison between three implementations of
 Typed Racket based on the same benchmarks, and a discussion of the major
 performance issues we uncovered.

Finally I want to give a special thanks to Sam Tobin-Hochstadt for developing
Typed Racket and for major improvements that took place alongside this work.

This was Ben Greenman explaining the main ideas of our paper.
Thanks for listenting, and please leave comments on the Journal of Functional
Programming web page.
If you have questions, post them too and I will respond in public.

