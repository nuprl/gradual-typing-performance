#!/bin/bash

# Independent parameters
JOBS=4
RKT=/home/ben/code/racket/fork/racket/bin
## Default iterations: try 10, run 30 if not-normal
#RKT=$(dirname `which racket`)

# Dependent parameters
TARGET=${1%/}
LOG=$TARGET.log

echo "### Running benchmarks for '"$TARGET"' ("$NUMITERS" iterations per config.)"
$RKT/raco make tools/data-lattice.rkt
$RKT/racket tools/setup-benchmark.rkt $TARGET
$RKT/racket tools/run.rkt -j $JOBS -r $RKT $TARGET | tee $LOG
echo "### Saved logfile to '"$LOG"'"
