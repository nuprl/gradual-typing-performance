This is an experiment in using boundaries as configurations instead of
using modules. This lets us build a potentially better prediction space.

Each configuration is based on an existing benchmark configuration. Namely
the one where a single module is typed. Like these for suffixtree:

  * configuration100000
  * configuration001000

For each one, we create a variant where only a single boundary is toggled
at a time:

  * configuration001000-0 (an outbound boundary, has a number)
  * configuration000100-a (an inbound boundary, has a letter)

Outbound means the typed module is exporting (i.e., use `unsafe-provide`
and a submodule) and inbound means the typed module is importing
(i.e., use `unsafe-require/typed`).

From this, we can build predictions for any real configuration using these
fake ones. The process is to sum up all the delta for boundaries that
contribute to the real configuration. A delta is the runtime for the
boundary configuration minus the untyped baseline.

TODO: better automation.
