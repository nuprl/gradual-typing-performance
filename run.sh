#!/bin/bash

export NUMITERS=5
export DATA=$1.rktd
export LOG=$1.log

echo "### Running benchmarks for '"$1"' ("$NUMITERS" iterations per configuration)"
racket tools/setup-benchmark.rkt $1
racket tools/run.rkt -i $NUMITERS -o $DATA $1 | tee $LOG
echo "### Saved output to '"$DATA"' and '"$LOG"'."
