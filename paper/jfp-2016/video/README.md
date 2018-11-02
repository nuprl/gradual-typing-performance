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

Hello
this is Ben Greenman speaking
and I'm here to tell what our team has learned
about performance and gradual typing systems.

In particular, I am going to explain a method we have developed to measure
the performance of a gradual typing system.
This method is a tool for language implementors
to understand the consequences of their decisions and to systematically
compare different gradual typing systems for the same underlying language.

To begin, let's see why performance is an issue for gradual typing.

In any programming language, we can think of a program as a collection of
components.
Some components may depend on one another. When we run a program,
values may cross these dependency edges.

In a gradually typed language, a developer is free to make some components
statically typed and other components dynamically typed.
Static and dynamic components may depend on one another;
in this case we call the edge between them a type boundary.

A type boundary is a channel of communication.
For example, a typed component may ask for an Integer value,
and an untyped component may respond with the untyped value 42.

In this case the type and value match, but in general
if we want to maintain a relation between types and values, then we need to
check for possible mis-communications.
For example, a typed component may ask for an Integer and receive the untyped
symbol N A N. To catch this miscommunication we need a runtime check.

These safety checks come at a run time cost.
The size of the cost depends on the interaction.
If a typed component expects a list, it may need to traverse the given value
to avoid a miscommunication.
If a typed component expects a function, it may choose to wrap the given value
in a proxy that monitors its behavior.

The bottom line is that type boundaries lead to a non-trivial runtime cost.

And so given a program, we would like to understand its boundaries.
More generally, given a gradual typing system we would like to understand its
performance implications.
To do this we need a method to measure performance systematically.

...

Our method starts by acknowledging that in a program with N components,
a developer has 2^N possibilities to choose from. They may start in the
untyped configurations, add types to a few components, and end somewhere
in the middle. We have no data to suggest that some configurations are more
likely, so the method must consider all possibilities.

Each configuration represents a working program. The next step is to measure
their running times.

Now we have an exponentially large dataset and the challenge is to find a
compact summary.
First, choose a baseline --- the untyped configuration is one possible choice.
Next, compute the overhead of each configuration relative to the baseline.
From these normalized numbers, it is clear that come configurations have
decent performance and others are miserable.
And so we condense the entire lattice into a ratio describing the number of
good configurations.

More precisely, the good configurations run no slower than some factor D
relative to the baseline. A language designer may instantiate the parameter
D with the largest slowdown they are willing to impose on users.

In summary
start with any program,
apply gradual typing,
explore all possibilities,
and count the proportion of D-deliverable configurations for your chosen
value of D

...

We have also developed a method for plotting the results.
On the x-axis we have values of D to some large upper bound.
On the y-axis we have the percent of D-deliverable configurations.
In the paper, we choose 20 as our upper bound.

For any program, we can now see how increasing the value of D increases the
proportion of D-deliverable configurations.
The line should ideally reach the top of the y-axis at a low value of D.
Generally, a large area under the curve is better.

...

One limitation of the method is that it requires an exponential number of
measurements.
In general a program of N components has 2-to-the-power-of-N configurations to
measure.
And so, we developed a technique to scale the method.

The idea is to sample a linear number of configurations and count the proportion
of D deliverable configurations in the sample.
Repeating this process for multiple samples leads to a statistically sound
approximation of the true proportion.

For details, see the papque to scale the method.

The idea is to sample a linear number of configurations and count the proportion
of D deliverable configurations in the sample.
Repeating this process for multiple samples leads to a statistically sound
approximation of the true proportion.

For details, see the paper.

...

There you'll find an empirical justification of the sampling method,
 an application of the original exhaustive method to Typed Racket,
 a direct comparison between three versions of Typed Racket,
 and a discussion of the major performance issues we uncovered.

Finally, I want to give a special thanks to Sam Tobin-Hochstadt,
for developing Typed Racket and for major improvements that took place
alongside our work.

This was Ben Greenman explaining our paper to you.
Thanks for listening and please leave comments on the
Journal of Functional Programming's web site. If you have questions, leave them
there and I will answer in public
