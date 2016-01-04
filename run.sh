#!/bin/bash

# Independent parameters
NUMITERS=4
JOBS=4
RKT=/home/ben/code/racket/6.2/bin/

# Dependent parameters
TARGET=${1%/}
LOG=$TARGET.log

echo "### Running benchmarks for '"$TARGET"' ("$NUMITERS" iterations per config.)"
$RKT/raco make tools/data-lattice.rkt
$RKT/racket tools/setup-benchmark.rkt $TARGET
$RKT/racket tools/run.rkt -j $JOBS -r $RKT -i $NUMITERS $TARGET >& $LOG
echo "### Saved logfile to '"$LOG"'"
