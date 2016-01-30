#!/bin/sh
#BSUB -J QUAD-P2
#BSUB -o quad_output_part_2_file
#BSUB -e quad_error_part_2_file
#BSUB -n 40
#BSUB -R "span[ptile=40]"
#BSUB -q ser-par-10g-2
#BSUB -cwd /scratch/takikawa.a

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

~/racket-6.2/bin/racket tools/run.rkt -o quad-results-part-2.rktd -m 5001 8000 -i 30 -j 19 quad

cp quad-results-part-2.rktd ~/

#####################################################
########DO NOT EDIT ANYTHING BELOW THIS LINE#########
#####################################################
rm $work/$tempfile1
rm $work/$tempfile2
#####################################################
########DO NOT EDIT ANYTHING ABOVE THIS LINE#########
#####################################################
