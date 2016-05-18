#!/bin/bash

echo "### Preparing to run ALL benchmarks. This may take over 6 months."
read -p "Are you sure you want to continue? " -n 1 -r
echo ""
if [[ $REPLY =~ ^[Yy]$ ]]
then
  raco gtp-run -v '6.5' \
    benchmarks/morsecode \
    benchmarks/fsm \
    benchmarks/forth \
    benchmarks/take5 \
    benchmarks/zordoz.6.3 \
    benchmarks/zombie \
    benchmarks/suffixtree \
    benchmarks/snake \
    benchmarks/tetris \
    benchmarks/synth \
    benchmarks/kcfa \
    benchmarks/lnm \
    benchmarks/mbta \
    benchmarks/acquire \
    benchmarks/dungeon
fi
#    fsmoo \
#    quadBG \
#    quadMB \
#    gregor \
#    sieve
