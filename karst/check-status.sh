# check-status.sh <RKT> <N>

# Check the status of running benchmarks
#  by counting the number of output files:
#  - for Racket version `<RKT>`
#  - with `<N>` iterations finished

if [ $# -eq 2 ]; then # need 2 arguments
  RACKET_VERSION=$1
  APPEND=$2
  # Same variables `run-benchmark.sh` needs
  #GTP="/N/u/bengree/Karst/gtp/benchmarks"
  GTP="../benchmarks"
  OUT="output-${RACKET_VERSION}.txt"
  CONFIG_GLOB="config*"

  # Count number of correctly-named output files with correct num. lines
  NUM_FINISHED=0
  for FN in $( find ${GTP} -name ${OUT} ); do
    if [ $( wc -l < ${FN} ) -eq ${APPEND} ]; then
      NUM_FINISHED=$(( 1 + ${NUM_FINISHED} ))
    fi
  done

  # Count all configurations
  TOTAL=0
  for FN in $( find ${GTP} -name "${CONFIG_GLOB}" ); do
    TOTAL=$(( 1 + ${TOTAL} ))
  done

  # Compute a ratio `NUM_FINISHED/TOTAL`
  OFFBY=0
  if [ ! ${TOTAL} -eq 0 ]; then
    if [ ! ${NUM_FINISHED} -eq 0 ]; then
      ACCUM=0
      while [ ${ACCUM} -lt ${TOTAL} ]; do
        ACCUM=$(( ${ACCUM} + ${NUM_FINISHED} ))
        OFFBY=$(( ${OFFBY} + 1 ))
      done
    fi
  fi
  echo "Finished ${NUM_FINISHED} of ${TOTAL} configurations (1/${OFFBY})"
else
  echo "Usage: check-status.sh <RACKET-VERSION> <NUM-LINES-EXPECTED>"
fi
