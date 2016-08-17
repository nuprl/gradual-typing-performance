
USER="bengree"
USER_DIR="/N/u/${USER}/Karst"
GTP="${USER_DIR}/gtp"
SAMPLER="${GTP}/karst/sampler"

NUM_NODES=1
NODE_TIME="00:00:10"
NODE_SCRIPT="${SAMPLER}/config-sampler-node.sh"

## -----------------------------------------------------------------------------

for i in $( seq 1 ${NUM_NODES} ); do
  qsub -k o -l nodes=1:ppn=16,walltime=${NODE_TIME} ${NODE_SCRIPT}
done
