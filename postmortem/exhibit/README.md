This folder is experimental.
These results should eventually be sorted and moved to an official `paper/` directory.

### Exhibits for meeting on 2015-08-28

- `B` an epsilon-change that improves tetris's worst case from 40 seconds to 1 second.
- `C` sorted variations for all benchmarks

### Exhibits for meeting on 2015-09-10

- `D` checking what profiler measures, tried slow contracts & slow functions
- `E` re-running with (-> any boolean) contracts disabled, see `postmortem/profile` for supplements
- `F` describe recent changes to MBTA and zordoz
- `G` a surprise. Made tests for vector & list contracts, saw that vector contracts are just less expensive.
      Really improves performance for `tetris`. Really slows down `snake` (but not `snake` contracts).
- `H` spot-checking other disabled contracts (partner to `E`)
