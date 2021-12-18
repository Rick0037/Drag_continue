# xMLC
# Guy Y. Cornejo Maceda, 27/01/2020
# Copyright: 2020 Guy Cornejo Maceda (gy.cornejo.maceda@gmail.com)
# CC-BY-SA


Machine Learning Control.
How to use the xMLC software.
This software is based on a Genetic Programming framework to build control laws for dynamical systems.

## Getting Started

Unzip the tar.gz file.

### Prerequisites

The software needs MATLAB or Octave.
No additional packages are needed.
This version has been developped on MATLAB version 9.5.0.944444 (R2018b) and Octave version 4.2.2.
Please contact gy.cornejo.maceda@gmail.com in case of error.

### Content
The main folder should contain the following folders and files:
- *README.md*
- *Initialization.m*, *Restart.m*, to initialize and restart the toy problem.
- *@MLC/*, *@MLCind/*, *@MLCpop/*, *@MLCtable/* contains the object definition files for the MLC algorithm.
- *MLC_tools/* contains functions and scripts that are not methods used by the MLC class objects.
- *ODE_Solvers/* contains other functions such ODE solvers
- *Plant/* contains all the problems and associated parameters. One folder for each problem. Default parameters are in *MLC_tools/*.
- *Compatibility/* contains functions and scripts for MATLAB/Octave compatibility.
- *Control_laws/* contains functions and scripts to be used for experiments.
- *save_runs/* contains the savings.

### Compatibility
To change the compatibility to MATLAB or Octave, go to the *Compatibility/* directory and execute the appropriate bash script.
If you are on Windows, please copy the files in the MATLAB or Octave folder to the adequate ones.

### Initialization and run
To start, run matlab in the main folder, then run Initialization.m to load the folders and class object.

```
Initialization;
```

A *mlc* object is created containing the toy problem.
The toy problem is the Generalized Mean-Field Model (GMFM).
To load a different problem, just specify it when the MLC object is created.

```
mlc=MLC('GMFM');
```

To start the optimization process of the toy problem, run the *go.m* method.
Run alone it process one generation of optimization.
For the first iteration, it will initialize the data base with new individiduals.
Then it will create the new generations by evolving the last population thanks to genetic operators.

```
mlc.go;
```

To run several generations, precise it.

```
mlc.go(10); %To run 10 generations.
```

## Post processing and analysis.

To visualize the best individual, use :

```
mlc.best_individual;
```

To visualize the learning process, use : 

```
mlc.convergence;
```



The current figure can be directly saved in save_runs/NameOfMyRun/Figures/ thanks to the following command:
```
mlc.print('NameOfMyFigure');
mlc.print('NameOfMyFigure',1); % to overwrite an existing figure
```
### Useful parameters

```
mlc.parameters.name = 'NameOfMyRun'; % This is the used to save;
mlc.parameters.Elitism = 1;
mlc.parameters.CrossoverProb = 0.3;
mlc.parameters.MutationProb = 0.4;
mlc.parameters.ReplicationProb = 0.7;
mlc.parameters.PopulationSize = 50; % Size of the population

```

### Save/Load

One can save and load one run.
/!\ When loading, the current mlc object will be overwritten, be careful!

```
mlc.save_matlab;
mlc.load_matlab('NameOfMyRun');
```

## Versioning

Version 0.9.6.1

Version 0.9.7 - Compatible with version 0.9.6.1
Correction :
 - Concatenation of evaluations: in case of a BadValue the concatenation was sometimes impossible.FIXED.
Remark :
 - During the pre-evaluation with the control points, a control law can be given a BadValue if the evaluations gives NaN or Inf. In case for example of sin(exp(exp(7))).
We choose to not remove the control law in question because this pre-evaluation is just an estimator.  
Modifications without impact:
 - mutate.m, crossover.m and create_indiv.m in @MLCind have been modified so they comprise MATLAB and Octave versions. Thus the corresponding files have been removed in Compatibility.

Version 0.9.7.1 - Compatible with 0.9.7
Changes : 
 - Dots are suppressed for * and / when ExtractBestIndividuals.m is used.
 - choose_operation.m takes the probabilities [pc,pm,pr] as argument now.
Correction :
 - Time dependent functions couldn't be used because the internal variables were not replaced before the evaluation. FIXED.

Version 0.9.7.2 - Slight correction to do for compatibility with earlier versions.
Correction:
 - In evaluate_pop.m, the cost of the individual in the population is given after all the evaluations are finished, thus two identical individuals will have the same cost in case of reevaluation.
This was not the case for system  with noise. Everything is OK for experiment.
 - In the parameters, 'Prestesting' has been corrected to 'Pretesting'.
/!\ In order to reuse savings from earlier version, please add the 'Pretesting' with the command:'mlc.parameters.Pretesting=0;', and don't forget to save the MLC object afterwards.

Version 0.9.7.3 - Compatible with 0.9.7.2
Improvement:
 - give.m : gives more information concerning the individual and according to the EstimatePerformance parameter.
 - convergence.m :  the plot with all the components is sorted following the relative ordering in each generation but the costs are computed according to the EstimatePerformance parameter.

Version 0.9.7.4 - Compatible with 0.9.7.2
Correction: 
 - strrep_cl.m has been corrected. Now time dependent functions can be used.

Version 0.9.7.5 - Compatible with 0.9.7.4
Correction (Thanks to Philipp Oswald):
 - mat2lips.m and exclude_introns.m have been corrected to accept operator indices starting from  number different than 1.
Update:
 - evaluate_indiv.m : gives to the _Plant_ the control law without the 'thresh' function that limits the control.
/!\ From now on, the limitation should be included in the xxx_problem.m file.

Version 0.9.7.6 - Compatible with 0.9.7.5
Correction (Thanks to Philipp Oswald)
 - readmylisp_to_evaluate.m same correction as in v0.9.7.5

## Author

* **Guy Y. Cornejo Maceda** 

## License

xMLC (Machine Learning Control) for taming nonlinear dynamics.
    Copyright (c) 2020, Guy Y. Cornejo Maceda (gy.cornejo.maceda@gmail.com)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Acknowledgments

* The author thanks Thomas Duriez and Ruying Li for the great help they provided by sharing their own code.
* The author also thanks Bernd R. Noack and Francois Lusseyran for their precious advice and guidance.



