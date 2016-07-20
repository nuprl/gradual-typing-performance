# This script assumes that all configurations for all interesting benchmarks
# have already been generated.

# -----------------------------------------------------------------------------
# --- Initialize variables

RACKET_VERSION="6.2"
# String, a valid Racket version like "6.3"

APPEND=0 #$2
# Natural, the number of iterations we should have already finished.
#   A "round" is over when all configs have (${APPEND} + 2) lines of output.

EXPECTED_LINES=$((APPEND + 2))

GTP="/N/u/bengree/Karst/gtp/benchmarks"
RKT="/N/u/bengree/Karst/racket/${RACKET_VERSION}/bin"
OUT="output-${RACKET_VERSION}.txt"

THE="benjaminlgreenman"
BOSS="gmail"
THEBOSS="${THE}@${BOSS}.com"

LOCKFILE="mutex.lock"

# -----------------------------------------------------------------------------

while [ 1 ]; do
  # --- Pick a random configuration to run, exit if there aren't any left
  NUM_CONFIGS_LEFT=0
  if [ ${APPEND} -eq 0 ]; then
    # echo "APPEND 0"
    for DIR in $(find ${GTP} -name "config*"); do
      # echo "DIR = ${DIR}"
      cd ${DIR};
      if [ ! -f ${OUT} ]; then
        # echo "OUT does not exist"
        NUM_CONFIGS_LEFT=$((NUM_CONFIGS_LEFT + 1));
      fi
      cd -;
    done
  else
    for FN in $(find ${GTP} -name ${OUT}); do
      if [ $(wc -l < ${FN}) -eq ${APPEND} ]; then
        NUM_CONFIGS_LEFT=$((NUM_CONFIGS_LEFT + 1));
      fi
    done
  fi
  # echo "NCL = ${NUM_CONFIGS_LEFT}"
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
    # echo "CONFIG INDEX = ${CONFIG_INDEX}"
    CONFIG_FILE=""
    if [ ${APPEND} -eq 0 ]; then
      for DIR in $(find ${GTP} -name "config*"); do
        if [ ! -f "${DIR}/${OUT}" ]; then
          CONFIG_INDEX=$((CONFIG_INDEX - 1))
          if [ ${CONFIG_INDEX} -eq 0 ]; then
            CONFIG_FILE="${DIR}/${OUT}"
            break
          fi
        fi
      done
    else
      for FN in $(find ${GTP} -name ${OUT}); do
        if [ $(wc -l < ${FN}) -eq ${APPEND} ]; then
          CONFIG_INDEX=$((CONFIG_INDEX - 1))
          if [ ${CONFIG_INDEX} -eq 0 ]; then
            CONFIG_FILE=$FN
            break
          fi
        fi
      done
    fi
    # echo "CFG FILE = ${CONFIG_FILE}"

    # --- Check if someone else already picked the file, otherwise do nothing
    if [ ! -z ${CONFIG_FILE} ]; then
      cd $(dirname ${CONFIG_FILE});
      if [ ! -f ${LOCKFILE} ]; then
        echo "Running: ${CONFIG_FILE}"
        touch ${LOCKFILE}
        $RKT/raco make main.rkt
        printf "(PID $$) (PBS_NODENUM ${PBS_NODENUM}) (PBS_JOBID ${PBS_JOBID}) ( (PBS_O_HOST ${PBS_O_HOST}) ::: " >> $OUT # No trailing newline
        $RKT/racket main.rkt >> $OUT
        rm ${LOCKFILE}
      fi
    fi
  fi
done

