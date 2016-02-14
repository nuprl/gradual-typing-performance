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
    zordoz.6.3 \
    zombie \
    kcfa \
    gregor \
    acquire \
    quadU \
    fsmv2 \
    fsmoo \
    forth \
    quad \
    sieve \
  ; do
    sh run.sh benchmark/$project
  done
fi
