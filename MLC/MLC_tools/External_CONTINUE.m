function External_CONTINUE(gen)
%% EXTERNAL_CONTINUE for in a continue Re number to build the MLC
cd ../
Initialization;

%% PinballMF is used to 
% mlc=MLC('PinballMF');
 mlc=MLC('Pinball')

 %% Load
   % mlc.load_matlab('PinballMF',['Gen',num2str(gen-1)]);
      mlc.load_matlab('Pinball47',['Gen',num2str(gen-1)]); 
      % the secound part is load as the 'save_runs/Pinball1/Gen0_matlab.mat' 
%% Complete
    matJ = External_matJ(mlc.parameters,gen);
   %  matJ = External_build_matJ(mlc.parameters,gen);
    complete_evaluation(mlc,gen,matJ);

%% Evolve
    evolve_population(mlc); 
    % evolve population will creative a next population and willsave it in the save   

%% Save
    mlc.save_matlab(['Gen',num2str(length(mlc.population)-1)]);
   exit
    %  save the mlc.mat as a Gen_Matlab.mat file .

