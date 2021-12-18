#!/bin/bash
# first argument is the number of the 'Run_' folder that will run the simulation
# second argument is the generation to evaluate
# third argument is the individual to evaluate

MATLAB_PATH=/usr/local/MATLAB/R2016b/bin/
Re=(30 60 80 100 120)
i=0
end=5
while [ "$i" != "$end" ]
do 
	##Change the sensor flow  
	## Go to good run folder
	cd Run_$1/
	./ChRe.sh ${Re[$i]}
	# Restart
	rm Code_Output/* #Clean before the simulation
	# cp Code_Output_Init/* Code_Output/ #CP the first time steps

	## Initialization
	./ReChoose_individual.sh $2 $3 #cycle set_type number_set

	## Run
	rm -f Evaluate_Barrier
	touch Evaluate_Barrier
	${MATLAB_PATH}/matlab -nodesktop -nosplash -r "ReControl(${Re[$i]});"
	#./UNS3 

	if [ -f "Covergent_Barrier" ];then
	# pkill UNS3
	rm -f Covergent_Barrier
	fi

	## Compute cost function
	cd ../MLC/Plant/Pinball/Cost_evaluation01/ 
	${MATLAB_PATH}/matlab -nodesktop -nosplash -r "Cost_Function(${Re[$i]},$1,$2,$3,600,750);"

	## Save sensors and restart
	cd ../../../..
	mv Run_$1/Sensor_feedback/Coe_sensors.dat $WORKDIR PinballMF/OUTPUT/SS/${Re[$i]}Set${2}_${3}sensors.dat
	cp Forces_Data/${Re[$i]}Gen${2}Ind${3}.mat $WORKDIR PinballMF/OUTPUT/Forces_Data/
	cp Costs/${Re[$i]}Gen${2}Ind${3}.dat $WORKDIR PinballMF/OUTPUT/Costs/
	i=$(($i+1))
done 
