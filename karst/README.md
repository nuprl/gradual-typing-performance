karst
===

Scripts for the Karst cluster.

- `again.rkt` resume where we left off yesterday
  - Writes to: `.again.rktd`
- `check-status.sh <RKT> <N>` count the number of output files for Racket version `<RKT>` with `<N>` iterations completed.
- `collect.rkt <VERSION>` gather results for one Racket version.
   Use `racket collect.rkt --help` to see options.
- `run-benchmark.sh` puts ONE node to work running configurations
- `setup-benchmark.sh` clears the old benchmark directory, sets up everything
- `qscript.sh` instructions for a node. This is what `run-benchmark.sh` starts running.


Getting Started
---

Workflow for a new cluster account:
1. Login to the cluster, download/install Racket, download/install this repo
2. Run the setup script
3. Run the `run` script for as many nodes as possible
4. ... wait 24 hours, repeat step 3 until done

