#!/bin/bash

echo "### Preparing to run ALL benchmarks. This may take over 6 months."
read -p "Are you sure you want to continue? " -n 1 -r
echo ""
if [[ $REPLY =~ ^[Yy]$ ]]
then
  for project in \
    gregor \
    kcfa \
    lnm \
    mbta \
    morsecode \
    quad \
    sieve \
    snake \
    suffixtree \
    synth \
    tetris \
    zordoz.6.2.900.15 \
  ; do
    sh run.sh $project
  done
fi
