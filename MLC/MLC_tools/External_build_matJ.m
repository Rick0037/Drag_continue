function matJ = External_build_matJ(MLC_parameters,Gen,Re)
%% Parameters
    PopSize = MLC_parameters.PopulationSize;
    PathExt = MLC_parameters.ProblemParameters.PathExt;
    gamma = MLC_parameters.ProblemParameters.gamma;
    
%% Allocation
    Jcomponents = NaN(PopSize,numel(gamma)+1);
    
%% Data loading
    for i=1:PopSize
        filename = [num2str(Re),'Gen',num2str(Gen),'Ind',num2str(i),'.dat'];
        %filename = ['Gen',num2str(Gen),'Ind',num2str(i),'.dat'];
        costfile = fullfile(PathExt,filename);
        if exist(costfile,'file')
            Jcomponents(i,:) = load(costfile,'-ascii');
        else
            Jcomponents(i,:) = MLC_parameters.BadValue*ones(1,numel(gamma)+1); %If the evaluation fails the value of 10+31 is given which a purposely high value
        end

    end
    matJ = Jcomponents;

disp('matJ computed!')

end
