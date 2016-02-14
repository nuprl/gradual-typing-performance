#!/bin/bash
#############################################################################
# Independent parameters
#JOBS=4
RKT=/home/ben/code/racket/6.4.0.5/bin
## Default iterations: try 10, run 30 if not-normal
#RKT=$(dirname `which racket`)

#############################################################################
# Dependent Parameters

## Default iterations: try 10, run 30 if not-normal

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
$RKT/racket tools/run.rkt -r $RKT $TARGET | tee $LOG
echo "### Saved logfile to '"$LOG"'"
