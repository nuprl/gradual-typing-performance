#!/bin/bash

export NUMITERS=1
export TARGET=${1%/}
export DATA=$TARGET.rktd
export LOG=$TARGET.log
export PNG=$TARGET.png

echo "### Running benchmarks for '"$TARGET"' ("$NUMITERS" iterations per configuration)"
racket tools/setup-benchmark.rkt $TARGET
racket tools/run.rkt -i $NUMITERS -o $DATA $TARGET | tee $LOG
racket tools/view.rkt -o $PNG $DATA
echo "### Saved logfile to '"$LOG"'"
echo "### Saved data to '"$DATA"'"
echo "### Saved figure to '"$PNG"'"
