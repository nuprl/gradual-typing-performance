#!/bin/bash

echo "### Preparing to run ALL benchmarks. This may take over 6 months."
read -p "Are you sure you want to continue? " -n 1 -r
echo ""
if [[ $REPLY =~ ^[Yy]$ ]]
then
  for project in \
    acquire \
    forth \
    fsm \
    fsmoo \
    zordoz.6.2 \
    zombie \
    tetris \
    synth \
    kcfa \
    gregor \
    quadBG \
    quadMB \
    sieve \
  ; do
    sh run.sh benchmarks/$project
  done
fi
