#!/bin/sh
#BSUB -J QUAD-COMPILE
#BSUB -o quad_compile-output_file
#BSUB -e quad_compile-error_file
#BSUB -n 40
#BSUB -R "span[ptile=40]"
#BSUB -q ser-par-10g-2
#BSUB -cwd /scratch/takikawa.a

# file staging
#cp -R ~/gradual-typing-performanace /scratch/takikawa.a/gradual-typing-performance

work=/scratch/takikawa.a/gradual-typing-performance

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

~/racket-6.2/bin/racket tools/run.rkt -c -j 39 quad

#####################################################
########DO NOT EDIT ANYTHING BELOW THIS LINE#########
#####################################################
rm $work/$tempfile1
rm $work/$tempfile2
#####################################################
########DO NOT EDIT ANYTHING ABOVE THIS LINE#########
#####################################################
