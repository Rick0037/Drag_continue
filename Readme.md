This folder is about the feedback control varying condiation learning 
Re = 30,60,80,100,120
tstart=500s, tend=750s. 
start compute cost at 600s

### Get start 

In continue learning we start the sumilation in 20 process, So we have 20 Run_$x   folder to run.

In this folder you can copy  and rename until 20 folders.


the way to start to the continue sumulations 

1. open one octave in each Run_x folder to run      startSimu.m      finally open 20 octave to wait

2. you can change the input  about the boundary and propertie and the population in Recontrol.m  in folder 1 

(also can maybe noise level in Run_1/Sensor_feedback/Sensor_MLC.m)

3. and copy to other Run_x  folder. You can open a terminal    ./changeInput.sh

4. change the xMLC extranel file population External_END.m External_CONTINUE.m 

5. start to run open a terminal    ./LGPC_in_continue.sh


### Some shell tools  

Here is the code for Bash Shell in varying learning condition.
    
    LGPC_in_continue.sh      is the for the start LGPC learning in continue 
    
    Evaluation.sh     is  for one generation's simulation 
    
    Multiple_Simulations.sh      is for one generations in several individual 

    One_Simulation.sh       is for one individual learning simulate in several Re 

And other is change some smalll tools 

    changeInput.sh      when we change conditions in Run_1 readly copy to other folder.

    clearnOutput.sh   clearn each folder Code_Output  snapshot 


### Some changes in xMLC  in varying  learning 

Some change at MATLAB xMLC file
    In MLC_tools/   path 
        1.create    External_matJ.m                 build cost value average five condations  
        2.create    External_CONTINUE.m      for varying learning  External_CONTINUE(gen);
        3.create    External_END.m                   for varying learning  External_END(gen);

    In Plants/  path
        1.change PInball_parameters.m       line58 :         ProblemParameters.EstimatePerformance = 'last';     used last
        2.change the cost funcation file    we add Re in function input. we used MLC/Plant/Cost_evaluation01/Cost_Function


### Data to Record

when we finaish one whole Run, we will  save in Run_record/

1. mkdir a folder like mkdir Datarun43 

2. and copy the output file PInballMF/OUTPUT (force, sensor ) under Datarun43

3. copy the population like MLC/save_runs/PInball43 the whole pop

4. copy the parameter file in MLC/Plant/Pinball_parameters.m  for this Run.


### Run_x    each  subfolder  detal 

1. Code_Input       ./new_flowstate/   it include the new flow start state in Re=30,60,80,100,120 from natural flow in 500s 

2. Code_Source      x_m_v_t3.f  line381: add    call system('touch Covergent_Barrier')  when the law is not convergant we can find and in the Bash Shell  One_Simulation.sh line 29 we can skill this individual continue Run this generation and next gen

3. MatlabV3     for see flow field and movie,  nVortFiled(Ntint, Ntend) ,  VortField(Nt),  Vortmove(Ntint, Ntend) save as  .avi

4. Sensor_feedback    Sensor_MLC.m compute sensor coefficent in snapshot and give value in law calculate Cd, Cl, Cldelay.m

5. Sensor_feedback  Coe_sensors.dat, Coe_sensor_30.dat et al. each have a initial coefficent value in 450s-500s 
Coe_sensor.dat have seven line    Time        Cd        Cl        Cl delay       Cd noise      Cl noise   Cl delay noise
and in Sensor_MLC  line 6 C=0; means the noise level    line 141   line 155 the folumate to calculate add noise part.

6. V3            Visualization file used in Tecplot to show the vortex flow filed.

7. ChRe.sh  ReChoose_individual.sh   some function change Re number and change gen ind be used in One_Simulation.sh

