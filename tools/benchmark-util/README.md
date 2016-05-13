benchmark-utils
===

Tools for performance lattice experiments.

Files here are _required_ by modules in the experiment.
These files help manage type definitions and boundaries.


API
---
#### `(require/typed/check form ...)`
  Same syntax as Typed Racket's [require/typed](docs.racket-lang.org/ts-reference/special-forms.html),
   but conditionally applies contracts to imported identifiers.
  Contracts are skipped when both the providing & requiring module are typed.

#### `(count-chaps)`
  If the current Racket installation is collecting chaperone information, will dump all statistics
   to the file `*count-chaps-out*` (also part of the API).
  Otherwise, will crash with an ugly error message.
