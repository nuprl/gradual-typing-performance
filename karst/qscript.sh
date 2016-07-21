# Usage: NONE
#  Do not call this script directly. Just let `run-benchmark.sh` do it.

# === HIGH-LEVEL BEHAVIOR =====================================================
# Loops forever,
# - picking a random configuration that has not been run yet (this round)
# - runs that configuration 1 time, records the result

# If interrupted by the server, we lose only 1 run & can clean the data after.

# === STATE ===================================================================
# All state is kept on the filesystem. In particular:
# - Input variable APPEND (argument $2) is the number of lines we expect
#    all output files to have at the start of "this round".
#   "This round" is over when all configurations have an output file with
#    APPEND+1 lines
# - Each configuration folder has an output file (variable OUT) marked with
#    a Racket version number.
#   This is the file we append to (or create if it's the first round).
# - A file LOCKFILE prevents race conditions

# === LOW-LEVEL BEHAVIOR ======================================================
# Here is what really happens:
# - Count the number of configuration directories that have either
#    (1) no output file or
#    (2) an output file with APPEND lines
# - Draw a random number `n` in range
# - Iterate over the configuration directories again, pick the `n`-th
#   (Make sure this directory still needs to run -- another node might
#    have finished it in the meantime.)
# - "Claim" the configuration by writing a lock file
# - Compile & run the configuration, append results to file

# Assumes that all configurations for all interesting benchmarks
#  have already been generated.

# =============================================================================
# --- start here --------------------------------------------------------------
# =============================================================================

RACKET_VERSION="6.2" # TODO should be $1
# String, a valid Racket version like "6.3"

APPEND=0 # TODO should be $2
# Natural, the number of iterations we should have already finished.
#   A "round" is over when all configs have (${APPEND} + 2) lines of output.

GTP="/N/u/bengree/Karst/gtp/benchmarks"
# Location of the GTP repo, used to find configurations

RKT="/N/u/bengree/Karst/racket/${RACKET_VERSION}/bin"
# Location of Racket executables, for this version

OUT="output-${RACKET_VERSION}.txt"
# Name of output file. Every configuration directory gets a file named ${OUT}

THE="benjaminlgreenman"
BOSS="gmail"
THEBOSS="${THE}@${BOSS}.com"
# For sending emails

LOCKFILE="mutex.lock"
# For claiming a configuration directory

# -----------------------------------------------------------------------------

while [ 1 ]; do # --- forever
  NUM_CONFIGS_LEFT=0 # --- Count remaining configuration directories
  if [ ${APPEND} -eq 0 ]; then
    # --- Count configs with NO output file
    for DIR in $(find ${GTP} -name "config*"); do
      cd ${DIR};
      if [ ! -f ${OUT} ]; then
        NUM_CONFIGS_LEFT=$((NUM_CONFIGS_LEFT + 1));
      fi
      cd -;
    done
  else
    # --- Count configs with an output file with the right number of lines
    for FN in $(find ${GTP} -name ${OUT}); do
      if [ $(wc -l < ${FN}) -eq ${APPEND} ]; then
        NUM_CONFIGS_LEFT=$((NUM_CONFIGS_LEFT + 1));
      fi
    done
  fi
  if [ ${NUM_CONFIGS_LEFT} -eq 0 ]; then
    # --- If no config directories, we must be done
    echo "DONE"
    echo "All configurations have ${APPEND} lines" | mail -s "[GTP:KARST] done" ${THEBOSS}
    break
  else
    # --- Choose a random config, indexed by their "find-order"
    CONFIG_INDEX=$(( ( RANDOM % NUM_CONFIGS_LEFT ) + 1 ))
    CONFIG_FILE=""
    if [ ${APPEND} -eq 0 ]; then
      # --- Again, look for files with no output
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
      # --- Otherwise, look for files with the right number of lines
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
    # --- Almost time to run something,
    # --- first be sure the config isn't taken by another node
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

