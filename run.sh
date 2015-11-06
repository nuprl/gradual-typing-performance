#!/bin/bash

# Independent parameters
NUMITERS=10
RKT=/home/ben/code/racket/6.2/bin/
#RKT=$(dirname `which racket`)

# Dependent parameters
TARGET=${1%/}
LOG=$TARGET.log

echo "### Running benchmarks for '"$TARGET"' ("$NUMITERS" iterations per config.)"
$RKT/racket tools/setup-benchmark.rkt $TARGET
$RKT/racket tools/run.rkt -r $RKT -i $NUMITERS $TARGET | tee $LOG
echo "### Saved logfile to '"$LOG"'"
