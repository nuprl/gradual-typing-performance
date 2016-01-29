#!/bin/sh
#BSUB -J QUAD-BENCHMARK
#BSUB -o quad_output_file
#BSUB -e quad_error_file
#BSUB -n 40
#BSUB -R "span[ptile=40]"
#BSUB -q ser-par-10g-2
#BSUB -cwd /scratch/takikawa.a

# file staging
cp -R ~/gradual-typing-performance /tmp/gradual-typing-performance

work=/tmp/gradual-typing-performance

#####################################################
########DO NOT EDIT ANYTHING BELOW THIS LINE#########
#####################################################
cd $work
tempfile1=hostlistrun
tempfile2=hostlist-tcp
echo $LSB_MCPU_HOSTS > $tempfile1
declare -a hosts
read -a hosts < ${tempfile1}
for ((i=0; i<${#hosts[@]}; i += 2)) ; 
do 
   HOST=${hosts[$i]}
   CORE=${hosts[(($i+1))]} 
   echo $HOST:$CORE >> $tempfile2
done
#####################################################
########DO NOT EDIT ANYTHING ABOVE THIS LINE#########
#####################################################

~/racket-6.2.0.3/bin/racket setup.rkt quad
~/racket-6.2.0.3/bin/racket tools/run.rkt -m 0 10000 -o quad-results.rktd -i 10 -j 39 quad

cp quad-results.rktd ~/

#####################################################
########DO NOT EDIT ANYTHING BELOW THIS LINE#########
#####################################################
rm $work/$tempfile1
rm $work/$tempfile2
#####################################################
########DO NOT EDIT ANYTHING ABOVE THIS LINE#########
#####################################################

rm -r /tmp/gradual-typing-performance
