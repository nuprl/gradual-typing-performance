VERSION=$1

#          gregor \
#          quadBG \
#          quadMB \
for BM in acquire \
dungeon \
forth \
fsm \
fsmoo \
kcfa \
lnm \
mbta \
morsecode \
sieve \
snake \
stack \
suffixtree \
synth \
take5 \
tetris \
zombie \
zordoz.6.2; do # zordoz.6.3 zordoz.6.5
sh run-benchmark.sh ${VERSION} ${BM}
done;
