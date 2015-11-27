gofish
======

Why does this take 10 second real time, but .5 seconds CPU time?


Changelog
---
- Split `gofish.rkt` into 4 files
- Parameterized functions instead of using global variables
  (init time included in the benchmark)
- Changed deck to a box, because set! wasn't changing the list
