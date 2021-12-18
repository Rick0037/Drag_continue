function MLC=load_matlab(MLC,Name,AuxName)
    % LOAD Load method for MLC.
    % Loads the MLC object (mlc) from the save_runs folder.
    % To load the MLC object : mlc.load('NameOfMyRun');
    %
    % Guy Y. Cornejo Maceda, 01/24/2020
    %
    % See also MLC, go, @MLC/load.

    % Copyright: 2020 Guy Cornejo Maceda (gy.cornejo.maceda@gmail.com)
    % CC-BY-SA
    % Name  is the fold of the question  
    % AuxName is the fold to the secound part the name of the .mat file defaut is MLC 
    % Can be use in the 'name of the run' 
%% Parameters
    if nargin<3,LoadName='MLC';else,LoadName=AuxName;end
    LoadMLC = ['save_runs/',Name,'/',LoadName,'_Matlab.mat'];
    tmp = load(LoadMLC);
    
%% Change properties
    prop = properties(MLC);
    for p=1:length(prop)
        new_prop = get(tmp.MLC,prop{p});
        set(MLC,prop{p},new_prop);
    end

%% Update properties
    MLC.parameters.Name = Name;
    
end
