
# Script for cluster nodes
# DO NOT INVOKE DIRECTLY

# Behavior:
# - claims a temporary directory
# - copies samples configs + a version of Racket to the new directory
# - runs each config a few times, records results in a single output file
# Should finish within 24 hours

# -----------------------------------------------------------------------------

ITERS=20

USER="bengree"
USER_DIR="/N/u/${USER}/Karst"
GTP="${USER_DIR}/gtp"
SAMPLER="${GTP}/karst/sampler"

RACKET_VERSION="6.6"
RACKET_TAR=

MY_RAC="${USER_DIR}/racket/${RACKET_VERSION}/racket/bin/rac"
MY_RACO="${MY_RAC}o"
MY_RACKET="${MY_RAC}ket"

CONFIGS_NAME="config-sampler"
CONFIGS_TAR="${SAMPLER}/${CONFIGS_NAME}.tar.gz"

MY_DIR="${SAMPLER}/${PBS_JOBID}"
MY_OUTPUT="${MY_DIR}/output.txt"

# -----------------------------------------------------------------------------

mkdir ${MY_DIR}
cd ${MY_DIR}
#cp ${CONFIGS_TAR} ${RKT_TAR} ${MY_DIR}
#tar -xzf "${MY_DIR}/${RACKET_VERSION}.tar.gz"
cp ${CONFIGS_TAR} ${MY_DIR}
tar -xzf "${MY_DIR}/${CONFIGS_NAME}.tar.gz"
# -- TODO shuffle the list
#for BM in ${MY_DIR}/${CONFIGS_NAME}/*/; do
#  for CONFIG in BM/config*/; do
#    TAG="$(basename ${BM} ) $( basename ${CONFIG} )"
#    MAIN="${CONFIG}/main.rkt"
#    printf "${TAG}\n" >> ${MY_OUTPUT}
#    ${MY_RACO} ${MAIN}
#    for i in $( seq 1 ${ITERS} ); do
#      ${MY_RACKET} ${MAIN} >> ${MY_OUTPUT}
#    done
#    printf "\n" >> ${MY_OUTPUT}
#  done
#done
