function External_evaluation_CONTINUE(gen,Re)
    % EXTERNAL_EVALUATION_CONTINUE continues the run.
    % To be used after the evaluation of the individuals of genetation GEN.
    % Retrieves the cost information and makes the population evolve.
    % New control laws are generated in the Population folder and ready to be evaluated.
    %
    % Guy Y. Cornejo Maceda, 01/24/2020
    %
    % See also External_evaluation_END, External_evaluation_START.

    % Copyright: 2020 Guy Cornejo Maceda (gy.cornejo.maceda@gmail.com)
    % CC-BY-SA

%Evolve_population_script
cd ../
Initialization;

%% PinballMF is used to 
% mlc=MLC('PinballMF');
 mlc=MLC('Pinball')

 %% Load
   % mlc.load_matlab('PinballMF',['Gen',num2str(gen-1)]);
      mlc.load_matlab('Pinball54',['Gen',num2str(gen-1)]); 
      % the secound part is load as the 'save_runs/Pinball1/Gen0_matlab.mat' 
%% Complete
    matJ = External_build_matJ(mlc.parameters,gen,Re);
   %  matJ = External_build_matJ(mlc.parameters,gen);
    complete_evaluation(mlc,gen,matJ);

%% Evolve
    evolve_population(mlc); 
    % evolve population will creative a next population and willsave it in the save   

%% Save
    mlc.save_matlab(['Gen',num2str(length(mlc.population)-1)]);
   exit
    %  save the mlc.mat as a Gen_Matlab.mat file .

