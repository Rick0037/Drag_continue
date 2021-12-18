#!/bin/bash
# Finish the first population and to evaluation and evolution 
# need to change the pop in the external_evaluation_continue(gen,Re)
# and finish the external_evaluation_finish(gen,Re) 
# change the folder name before anything! 

pop=1
MATLAB_PATH=/usr/local/MATLAB/R2016b/bin/

#maxpop is the final pop 
maxpop=10
while [ "$pop" != "$maxpop" ]
do
./Evaluation.sh $pop
cd MLC/MLC_tools/
${MATLAB_PATH}/matlab -nodesktop -nosplash -r "	External_CONTINUE($pop);"
cd ../../
	pop=$(($pop+1))
done
./Evaluation.sh $pop
cd MLC/MLC_tools/
${MATLAB_PATH}/matlab -nodesktop -nosplash -r "	External_END($pop);"
