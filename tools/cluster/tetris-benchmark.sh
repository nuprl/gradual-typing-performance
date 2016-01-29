#!/bin/sh
#BSUB -J TETRIS
#BSUB -o tetris_output_file
#BSUB -e tetris_error_file
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

~/racket-6.2/bin/racket setup.rkt tetris
~/racket-6.2/bin/racket tools/run.rkt -c -j 39 tetris
~/racket-6.2/bin/racket tools/run.rkt -i 30 -j 19 tetris

cp tetris*.rktd ~/

#####################################################
########DO NOT EDIT ANYTHING BELOW THIS LINE#########
#####################################################
rm $work/$tempfile1
rm $work/$tempfile2
#####################################################
########DO NOT EDIT ANYTHING ABOVE THIS LINE#########
#####################################################

rm -r /tmp/gradual-typing-performance
