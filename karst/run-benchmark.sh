VERSION=$1
BMNAME=$2

GTP="/N/u/bengree/Karst/gradual-typing-performance"
ITERS=3

BENCHMARK="${GTP}/benchmarks/${BMNAME}"
RKT="/N/u/bengree/Karst/racket/${VERSION}/bin"

${RKT}/racket ${GTP}/tools/setup-benchmark.rkt ${BENCHMARK};

### cd $RKT; echo "hash: ${git rev-parse HEAD}"; cd -;
for CFG in ${BENCHMARK}/benchmark/configuration*; do
  cd ${CFG};
  echo "### benchmark: ${BMNAME}"                                     >> qscript;
  echo "### racket: ${VERSION}"                                       >> qscript;
  echo "cd "`pwd`                                                     >> qscript;
  echo ${RKT}"/raco make -v main.rkt"                                 >> qscript;
  echo 'for i in `seq '${ITERS}'`; do '${RKT}"/racket main.rkt; done" >> qscript;
  qsub -k o -l nodes=1:ppn=1,walltime=24:00:00 qscript;
  cd -;
done;
