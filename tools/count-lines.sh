#!/usr/bin/env bash

projects=(sieve echo morse-code mbta suffixtree zordoz kcfa
          synth tetris snake gregor quad)

for p in ${projects[@]}
do
  echo "$p typed"
  wc -l $p/typed/*.rkt | grep total
  echo "$p untyped"
  wc -l $p/untyped/*.rkt | grep total
  echo "$p all others"
  wc -l $p/both/*.rkt $p/base/*.rkt | grep total
done
