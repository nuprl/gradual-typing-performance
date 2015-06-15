lmn
===

Pictures to visualize L-M-N results.

Definitions
-----------
From the paper

- `N-deliverable` overhead no worse than `(N *100)%` slowdown over untyped.
- `N/M-usable` overhead worse than `(N*100)%` but better than `(M*100)%`
- `L-step N/M-usable` reach `N/M-usable` after at most `L` type conversion steps.


Parameters
----------
Before using, be clear about:
- type conversion step
- 


Install
-------
To run the scripts, install Python & Racket.


Usage
-----
To use any script, TODO


Summary
-------
Main files:
- `NM-histogram` given a list of `.rktd` files, create a histogram showing the number
  of `N-deliverable` and `N/M-usable` variations across all data.
- `LNM-line` given a list of `.rktd` files, create a multi-line graph showing
  the number of `N-deliverable` or `N/M-usable` configurations for each project
  as `L` decreases.

Support files:
- `private/rktd-cat.rkt` catenate the data from a list of `.rktd` files into a single `.tab` spreadsheet.
