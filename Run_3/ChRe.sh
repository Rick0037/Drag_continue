#!/bin/bash
sed -i -e "s/.*\(.0  50.0\)/$1.0  \1/" ./Code_Input/PROPERTIES
cp Sensor_feedback/Coe_sensors_$1.dat Sensor_feedback/Coe_sensors.dat
cp Code_Input/new_flowstate/Restart_unsteady_$1 Code_Input/Restart_unsteady
