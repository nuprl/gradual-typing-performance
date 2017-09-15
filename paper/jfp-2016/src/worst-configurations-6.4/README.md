worst-configurations-6.4
===

Copies of the slowest configurations on Racket v6.4

Also, results of running the:
- contract-profile
- chaperone counter
- module boundary tool
on each.

Code
---

Each directory in this folder correspond to a benchmark.
These directories contain 2 folders:
- `base/` contains "shared" and "data" files common to all configurations
- `[01]*/` contains code for the worst configuration

Term: "configuration folder" refers to any directory that matches the glob `*/[01]*/`


Contract Profile
---

Each directory in this folder has a file `contract-profile.txt`.
This file contains the output of running:

```
  raco contract-profile main.rkt
```

from the configuration folder on Racket v6.4.
These files report the total running time, running time attributed to contracts,
and the time spent in the top few expensive contracts.


Chaperone Counter
---

Each directory in this folder has a file `performance-stats.txt` that contains two
lines of data.
- First line is the output of running `vector-set-performance-stats!` after
  the benchmark finished
- Second line is a prefab struct that summarizes the number of primitive
  chaperones created and applied during execution
  (The chaperone-profiling machinery is adapted from OOPSLA'12)
A comment above the second line explains the data.


Module Boundary
---

Each directory in this folder contains a file `module-boundary.txt`.
This is the result of instrumenting the **untyped** configuration with print
statements every time a **static** procedure-call-across-a-boundary gets
executed. It's an approximate count of how frequently the benchmark crosses
module boundaries.
