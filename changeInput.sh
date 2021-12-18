#!/bin/bash
# should change the pop in the Run_1 and copy to other folder 
pl=2
folder=21

while [ "$pl" != "$folder" ]
do
	cp Run_1/Code_Input/PROPERTIES Run_$pl/Code_Input/
	cp Run_1/Code_Input/Restart_unsteady Run_$pl/Code_Input/

	cp Run_1/ReControl.m Run_$pl/

	cp Run_1/Sensor_feedback/Coe_sensors.dat Run_$pl/Sensor_feedback/
	cp Run_1/Sensor_feedback/Sensor_MLC.m Run_$pl/Sensor_feedback/

	pl=$(($pl+1))

done

