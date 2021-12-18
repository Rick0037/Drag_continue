#!/bin/bash
#PBS -N Geni_1
#PBS -l nodes=1:ppn=8,mem=8gb
#PBS -l walltime=24:00:00
#PBS -o my_job_i_1.out
#PBS -e my_job_i_1.err	
#PBS -V

# Only one parameter 
# First parameters is Population. 


#Variables globales

OUTPUT_DIRECTORY=${WORKDIR}PinballMF/OUTPUT/

#PARAMETERS
#./ChangeRe.sh $Re #Re (100)
./ChangeTmax.sh 750 

#EVALUATION
./Multiple_Simulations.sh 1 $1 1 5 &
./Multiple_Simulations.sh 2 $1 6 10 &
./Multiple_Simulations.sh 3 $1 11 15 & 
./Multiple_Simulations.sh 4 $1 16 20 &
./Multiple_Simulations.sh 5 $1 21 25 &
./Multiple_Simulations.sh 6 $1 26 30 &
./Multiple_Simulations.sh 7 $1 31 35 &
./Multiple_Simulations.sh 8 $1 36 40 &
./Multiple_Simulations.sh 9 $1 41 45 &
./Multiple_Simulations.sh 10 $1 46 50 &
./Multiple_Simulations.sh 11 $1 51 55 &
./Multiple_Simulations.sh 12 $1 56 60 &
./Multiple_Simulations.sh 13 $1 61 65 &
./Multiple_Simulations.sh 14 $1 66 70 &
./Multiple_Simulations.sh 15 $1 71 75 &
./Multiple_Simulations.sh 16 $1 76 80 &
./Multiple_Simulations.sh 17 $1 81 85 &
./Multiple_Simulations.sh 18 $1 86 90 &
./Multiple_Simulations.sh 19 $1 91 95 &
./Multiple_Simulations.sh 20 $1 96 100 &

wait

