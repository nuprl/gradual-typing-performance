#!/bin/bash

echo "### Preparing to run ALL benchmarks. This may take over 6 months."
read -p "Are you sure you want to continue? " -n 1 -r
echo ""
if [[ $REPLY =~ ^[Yy]$ ]]
then
  for project in \
    lnm \
    mbta \
    morsecode \
    snake \
    suffixtree \
    synth \
    tetris \
    zordoz.6.2.900.15 \
    zombie \
    forth \
    gregor \
    kcfa \
    quad \
    sieve \
  ; do
    sh run.sh $project
  done
fi
