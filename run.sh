#!/bin/bash

export NUMITERS=1
export DATA=$1.rktd
export LOG=$1.log
export PNG=$1.png

echo "### Running benchmarks for '"$1"' ("$NUMITERS" iterations per configuration)"
racket tools/setup-benchmark.rkt $1
racket tools/run.rkt -i $NUMITERS -o $DATA $1 | tee $LOG
racket tools/view.rkt -o $PNG $DATA
echo "### Saved logfile to '"$LOG"'"
echo "### Saved data to '"$DATA"'"
echo "### Saved figure to '"$PNG"'"
