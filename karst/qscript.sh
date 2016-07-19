# This script assumes that all configurations for all interesting benchmarks
# have already been generated.

# -----------------------------------------------------------------------------
# --- Initialize variables

VERSION="6.2"
# String, a valid Racket version like "6.3"

APPEND=1 #$2
# Natural, the number of iterations we should have already finished.
#   A "round" is over when all configs have (${APPEND} + 2) lines of output.

EXPECTED_LINES=$((APPEND + 2))

GTP="/N/u/bengree/Karst/gradual-typing-performance/benchmarks/morsecode"
RKT="/N/u/bengree/Karst/racket/${VERSION}/bin"
OUT="output-${VERSION}.txt"

THE="benjaminlgreenman"
BOSS="gmail"
THEBOSS="${THE}@${BOSS}.com"

LOCKFILE="mutex.lock"

# -----------------------------------------------------------------------------

while [ 1 ]; do
  # --- Pick a random configuration to run, exit if there aren't any left
  NUM_CONFIGS_LEFT=0
  for FN in $(find ${GTP} -name ${OUT}); do
    if [ $(wc -l < ${FN}) -eq ${EXPECTED_LINES} ]; then
      NUM_CONFIGS_LEFT=$((NUM_CONFIGS_LEFT + 1));
    fi
  done
  if [ ${NUM_CONFIGS_LEFT} -eq 0 ]; then
    echo "DONE"
    echo "All configurations have ${APPEND} lines" | mail -s "[GTP:KARST] done" ${THEBOSS}
    break
  else
    # --- Choose one of the configs.
    #     RACE CONDITION: it's possible that the index isn't valid now
    #                      because another node also picked it.
    #                     If so, just restart the while loop.
    CONFIG_INDEX=$(( ( RANDOM % NUM_CONFIGS_LEFT ) + 1 ))

    CONFIG_FILE=""
    for FN in $(find ${GTP} -name ${OUT}); do
      if [ $(wc -l < ${FN}) -eq ${EXPECTED_LINES} ]; then
        CONFIG_INDEX=$((CONFIG_INDEX - 1))
        if [ ${CONFIG_INDEX} -eq 0 ]; then
          CONFIG_FILE=$FN
          break
        fi
      fi
    done

    # --- Check if someone else already picked the file, otherwise do nothing
    cd $(dirname ${CONFIG_FILE});
    if [ ! -f ${LOCKFILE} ]; then
      touch ${LOCKFILE}
      $RKT/raco make main.rkt
      printf "(PID $$) (PBS_NODENUM ${PBS_NODENUM}) (PBS_JOBID ${PBS_JOBID}) ( (PBS_O_HOST ${PBS_O_HOST}) ::: " >> $OUT # No trailing newline
      $RKT/racket main.rkt >> $OUT
      rm ${LOCKFILE}
    fi
  fi
done

