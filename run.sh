#!/bin/bash
#############################################################################
# Parameters
RKT=/home/ben/code/racket/6.2/bin

## Default jobs: compute number of cores, divide by 2
if hash nproc 2>/dev/null; then
  CORES=$(nproc)
elif hash sysctl 2>/dev/null; then
  CORES=$(sysctl -n hw.physicalcpu)
else
  CORES=2 # So we get 1 job
fi
JOBS=$((CORES / 2))

TARGET=${1%/}
LOG=$TARGET.log

###############################################################################
## Main script
echo "### Running benchmarks for '"$TARGET"'"
if test $NUMITERS; then
 echo "### ("$NUMITERS" iterations per config.)"
fi
$RKT/racket tools/setup-benchmark.rkt $TARGET
$RKT/racket tools/run.rkt -j $JOBS -r $RKT $TARGET | tee $LOG
echo "### Saved logfile to '"$LOG"'"
