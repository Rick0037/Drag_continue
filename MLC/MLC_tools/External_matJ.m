function matJ = External_matJ(MLC_parameters,Gen)
%% Parameters
    PopSize = MLC_parameters.PopulationSize;
    PathExt = MLC_parameters.ProblemParameters.PathExt;
    gamma = MLC_parameters.ProblemParameters.gamma;
    Re=[30,60,80,100,120];
%% Allocation
    Jcomponents = zeros(PopSize,6*(numel(gamma)+1));
    
%% Data loading
    for i=1:PopSize
        for j=1:length(Re)
            filename = [num2str(Re(j)),'Gen',num2str(Gen),'Ind',num2str(i),'.dat'];
            costfile = fullfile(PathExt,filename);
            if exist(costfile,'file')
                Jcomponents(i,(7*j+1):(7*j+7)) = load(costfile,'-ascii');
            else
                Jcomponents(i,(7*j+1):(7*j+7)) = MLC_parameters.BadValue*ones(1,numel(gamma)+1); 
                %Jcomponents(i,(7*j+1):(7*j+7)) = MLC.parameters.BadValue*ones(1,numel(gamma)+1); 
            end
            Jcomponents(i,1:7)=Jcomponents(i,1:7)+Jcomponents(i,(7*j+1):(7*j+7));
        end
        Jcomponents(i,1:7)=Jcomponents(i,1:7)/length(Re);
    end
    matJ = Jcomponents(:,1:7);

disp('matJ computed!')

end
