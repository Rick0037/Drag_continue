#! /bin/bash
# first argument is the number of the 'Run_' folder that will run the simulation
# second argument is the generation to evaluate
# third argument is the first individual to evaluate
# fourth argument is the last individual to evaluate
# fifth argument is the Re number

indiv_to_evaluate=$(($4-$3+1))
cmpt=0

while [ "$cmpt" != "$indiv_to_evaluate" ]
do
	./One_Simulation.sh $1 $2 $(($3+$cmpt)) 
	cmpt=$(($cmpt+1))
done


