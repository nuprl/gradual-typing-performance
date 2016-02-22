Measuring Boundaries
--------------------

Directly measuring the full lattice of configurations of a program is
intractable in the program size (O(2^n) for n modules).
This fact is at odds with applying our methodology to large Typed
Racket programs or Micro-Gradual Typed programs.

However, we hypothesize that performance of gradually typed programs
can be understood as deriving from the prescence of "boundaries"
between incompatibly typed components and the optimization of typed
components.

Moreover, we hypothesize that the performance cost of different
boundaries are roughly independent, that is, the presence of one
boundary has at most a small effect on the performance contributed by
another.

A consequence is that we should be able to accurately predict
configuration performance based on measurements of individual
boundaries and optimizations.

If correct, this would necessitate far fewer measurements (O(n^2) or
O(m) for n modules, m module imports).

Model
-----

Following previous work on performance prediction (TODO: cite), we
model performance of a configuration as a sum of the costs/benefits of
its component *features*.

The two features we consider are typed optimization and type-induced
contract boundaries.
Performance is modeled as follows

P(configuration) = P(untyped) + Σ(feature f ∈ configuration) P(f) + Σ(features f,g ∈ configuration) (P(f#g)) + ...

where P(f) is the performance cost/benefit attributed to a feature f
and P(f#g) is the performance attributed to the *interaction* of
features f and g.

We hypothesize that the interaction costs are dominated by the feature
costs and are thus negligible, reducing the formula to

P(configuration) = P(untyped) + Σ(feature f ∈ configuration) P(f)

Experimental Setup
------------------

To test our model, we need to be able to measure boundaries and
optimizations.

1. To measure the benefit of typed optimization, we measure the
   performance of a configuration in which one module is typed, but
   all boundary contracts are disabled.

2. To measure boundary contracts, we run a configuration in which one
   module is typed and all but one of the contract boundaries are
   (unsafely) disabled.

Typed optimization can be disabled by a simple change to the file, so
the only technical challenge is to selectively disable contract
boundaries.

There are 2 cases, depending on if the requirer or requiree is typed.

1. If typed module A requires untyped module B, then we can simply use
   the `unsafe-require/typed` form instead of `require-typed`.

2. If untyped module A requires typed module B, then we can add a
   submodule to B in which all exports of B are exported using the
   `unsafe-provide` form instead of `provide`. Then A requires from
   B's submodule.

Therefore we propose the following procedure.
1. Add to every typed module a submodule "unsafe" that
   `unsafe-provide`s all of its parent module's `provide`s.
2. Add a file "boundary-overrides.rktd" to these "fake" configurations
   that says which boundaries should be present.
3. Add cases to `require/typed/check` that check for the presence of
   the boundary-overrides file and acts as described above.

Hypothesis
----------

Our hypothesis is that the predictions for modules, produced by our
model with the measurements performed as above will be "good".

It is not clear to us what "good" is.
"good" should mean good enough for our application, which is
performance evaluation of gradual-typing programming languages.
Most relevant measures are the 99th and 100th percentile of prediction
error.
