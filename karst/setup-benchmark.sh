GTP="/N/u/bengree/Karst/gtp"
RKT="/N/u/bengree/Karst/racket/6.5/bin"

find ${GTP} -name "benchmark" | xargs rm -r

for BM in acquire \
dungeon \
gregor \
forth \
fsm \
fsmoo \
kcfa \
lnm \
mbta \
morsecode \
quadBG \
quadMB \
sieve \
snake \
stack \
suffixtree \
synth \
take5 \
tetris \
zombie \
zordoz.6.2 \
zordoz.6.3 \
zordoz.6.5; do
${RKT}/racket ${GTP}/tools/setup-benchmark.rkt ${GTP}/benchmarks/${BM}
done;
