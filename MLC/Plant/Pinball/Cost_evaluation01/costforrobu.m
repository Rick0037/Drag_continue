function J_out = costforrobu(Re,runN,gen,indiv,tinit,tend)
% name = 'b0';
do_you_want_to_save = 1;
% tinit = 0;
% tend = 600;
tstep = 0.1;

%% Load the set
%    SetName = [num2str(cycle),'_',set_type];
%    LOAD_SET = load(['../../../save_runs/MCDS1/Sets/Set',SetName,'.mat']);
%    Set = LOAD_SET.Set;
%    ID = Set{number_set,1};

%% Parameters
    name = [num2str(Re),'Gen',num2str(gen),'Ind',num2str(indiv)]; % ����͸������ֵת�����ַ� 
    direc = ['../../../../Run_',num2str(runN),'/Code_Output/'];
    Ninit = 10*tinit; % ��ʼʱ���
    Nend = 10*tend; % ��ֹʱ���
    Nstep = 10*tstep; % ÿ���Ĳ���
    NTStp = Nend-Ninit+1; % һ�����ٲ�
    Re = 100; %��ŵ��
    R = 0.5;  %�뾶�ĳ���
    dt = 0.1;%�������tstep һ����ʱ����
%     save_directory = 
% �����Ŀ¼
    path=['Cost_Data/steady_solution_',num2str(Re)];
%% Load steady flow
	steady_solution = load(path); %��ȡ��̬�������
    steady_velocity = steady_solution(:,1:2); % ������Ľ��еĵ�һ�к͵ڶ�����ȡ����

%% Load grid elements
    elem = load('Cost_Data/elem.dat','-ascii'); % ��ȡ��Ԫ
    grid2 = load('Cost_Data/Grid2.dat','-ascii');% ��ȡ���
    %Load the area 
    area = load('Cost_Data/area.dat','-ascii');

%% Load symmetry files  ��ȡ�ԳƵ����
    DataSym = load('Cost_Data/NodesSymInterp');
    W = DataSym.bary_weight_sym; % ¼ȡȨ��
    TriIDX = reshape(transpose(DataSym.triangle_indices_sym),[],1); % transpose ����ת�� reshape ������б任��1�еľ���

%% Load data to compute forces �������÷�ʽ
    load 'Cost_Data/Mesh_Data'

%% Load Run and compute norm
    % Allocation
        % To compute Ja
              distance_to_steady2 = NaN(Nend-Ninit+1,1);
           %
            %distance_to_steady = NaN(Nend-Ninit+1,1);% ��nstep���������
        %  U(VxVy x nodes x corner x time)
            Us1 = zeros(2,length(s1),NTStp); % length ���������к����нϴ��ֵ
            Us2 = zeros(2,length(s2),NTStp);
            Us3 = zeros(2,length(s3),NTStp);

            Um1 = zeros(NTStp,2);
            Um2 = zeros(NTStp,2);
            Um3 = zeros(NTStp,2);
        % To compute the pressure force
            Pressure1 = zeros(NPtsCyl(1),NTStp);
            Pressure2 = zeros(NPtsCyl(2),NTStp);
            Pressure3 = zeros(NPtsCyl(3),NTStp);
        % Save U
            U = zeros(8633,NTStp);
            V = zeros(8633,NTStp);
        % To compute symmetry
            Uysym = zeros(NTStp,length(TriIDX));


    % For loop
        counter = 1;
        for N=Ninit:Nstep:Nend
            if N<10
                id1 = '00000';
            elseif N<100
                id1 = '0000';
            elseif N<1000
                id1 = '000';
            elseif N<10000
                id1 = '00';
            elseif N<100000
                id1 = '0';
            end

        % index 2
            id2 = num2str(N); % 
        % load
            flow = load([direc,'Flow.',id1,id2]);
        
        % To compute Ja
           % in another version
             % flow_velocity = flow(:,1:2);
            % distance_to_steady(counter) = norm(flow_velocity-steady_velocity,2);  %���ؾ��������ֵ
            FlowDiff = flow(:,1:2)-steady_velocity;
                UFD = FlowDiff(:,1);
                VFD = FlowDiff(:,2);
            UFDT = (UFD(elem(:,1),:)+UFD(elem(:,2),:)+UFD(elem(:,3),:))/3;
            VFDT = (VFD(elem(:,1),:)+VFD(elem(:,2),:)+VFD(elem(:,3),:))/3;
            % Integrale
            distance_to_steady2(counter) = sum((UFDT.^2+VFDT.^2).*area,1);
           
            
        %  PRESSURE EXTRACTION from the flow file
            Pressure1(:,counter) = flow(OnTheCyl1,3);
            Pressure2(:,counter) = flow(OnTheCyl2,3);
            Pressure3(:,counter) = flow(OnTheCyl3,3);

        %  SPEED EXTRACTION from the flow file  
            % At the external corner of the triangle
               Us1(:,:,counter) = (flow(s1,1:2))';
               Us2(:,:,counter) = (flow(s2,1:2))';
               Us3(:,:,counter) = (flow(s3,1:2))';

            % At the surface of each cylinder   
               Um1(counter,:) = flow(OnTheCyl1(1),1:2);
               Um2(counter,:) = flow(OnTheCyl2(1),1:2);
               Um3(counter,:) = flow(OnTheCyl3(1),1:2);
               
         % Velocity extraction to compute fluctuation energy
              U(:,counter) = flow(:,1);
              V(:,counter) = flow(:,2);
              
        % Uysym (columns=points - lines=time)
              Uysym(counter,:) = V(TriIDX,counter);             
              
        counter = counter+1;
        end

%% Distance to steady solution
    % 
    ja = distance_to_steady2;
    % ja = distance_to_steady.^2;
    Ja = mean(ja);
    
%% Actuation power
% Scalar product for each node
    % Initialization
    Ust1 = zeros(length(s1),NTStp); Usn1 = zeros(length(s1),NTStp);
    Ust2 = zeros(length(s2),NTStp); Usn2 = zeros(length(s1),NTStp);
    Ust3 = zeros(length(s3),NTStp); Usn3 = zeros(length(s1),NTStp);

    % Scalar product for each node
    Ust1(:,:) = sum(Us1.*Tvectm1');
    Ust2(:,:) = sum(Us2.*Tvectm2');
    Ust3(:,:) = sum(Us3.*Tvectm3');
    
     Nvectm1 = [Tvectm1(:,2),-Tvectm1(:,1)];
     Nvectm2 = [Tvectm2(:,2),-Tvectm2(:,1)];
     Nvectm3 = [Tvectm3(:,2),-Tvectm3(:,1)];
     
    Usn1(:,:) = sum(Us1.*Nvectm1');
    Usn2(:,:) = sum(Us2.*Nvectm2');
    Usn3(:,:) = sum(Us3.*Nvectm3');
    
        NvectOTC1 = [TvectOTC1(:,2),-TvectOTC1(:,1)];
        NvectOTC2 = [TvectOTC2(:,2),-TvectOTC2(:,1)];
        NvectOTC3 = [TvectOTC3(:,2),-TvectOTC3(:,1)];
    Umt1 = sum(Um1.*TvectOTC1,2); Umn1 = sum(Um1.*NvectOTC1,2);
    Umt2 = sum(Um2.*TvectOTC2,2); Umn2 = sum(Um2.*NvectOTC2,2);
    Umt3 = sum(Um3.*TvectOTC3,2); Umn3 = sum(Um3.*NvectOTC3,2);

   
    % dnUt (scalar product: gradUt.Nvect)
    dnUt1 = zeros(length(s1),NTStp); dnUn1 = zeros(length(s1),NTStp);
    dnUt2 = zeros(length(s2),NTStp); dnUn2 = zeros(length(s1),NTStp);
    dnUt3 = zeros(length(s3),NTStp); dnUn3 = zeros(length(s1),NTStp);

    dnUt1(:,:) = (Ust1-Umt1')./h1;
    dnUt2(:,:) = (Ust2-Umt2')./h2;
    dnUt3(:,:) = (Ust3-Umt3')./h3;

    dnUn1(:,:) = (Usn1-Umn1')./h1;
    dnUn2(:,:) = (Usn2-Umn2')./h2;
    dnUn3(:,:) = (Usn3-Umn3')./h3;
    % Torque
    % Torque1 = R*(1/Re)*dS(1)*(sum(dnUt1))';
    % Torque2 = R*(1/Re)*dS(2)*(sum(dnUt2))';
    % Torque3 = R*(1/Re)*dS(3)*(sum(dnUt3))';
    
    Torque1 = R*(1/Re)*dS(1)*(sum(dnUt1 - Umt1'/R))';
    Torque2 = R*(1/Re)*dS(2)*(sum(dnUt2 - Umt2'/R))';
    Torque3 = R*(1/Re)*dS(3)*(sum(dnUt3 - Umt3'/R))';

    
    % Speed on the cylinders
    Umt = [Umt3,Umt2,Umt1];
    Torque = [Torque3,Torque2,Torque1];
    v1 = Umt(:,1)/R;
    v2 = Umt(:,2)/R;
    v3 = Umt(:,3)/R;
    
    % Jb
    jb = -(v1.*Torque(:,1) + v2.*Torque(:,2) + v3.*Torque(:,3));
    Jb = mean(jb);
    
%% Pressure and viscous force
%  Allocation
    % pressure 
       Fp1 = zeros(2,NTStp);
       Fp2 = zeros(2,NTStp);
       Fp3 = zeros(2,NTStp);
    % viscous
       Fv1 = zeros(2,NTStp);
       Fv2 = zeros(2,NTStp);
       Fv3 = zeros(2,NTStp);

%  Loop
  counter = 1;
   for p=Ninit:Nend
      % pressure
      Fp1(:,counter) = -dS(1)*sum([Pressure1(:,counter).*Nvect1(:,1),Pressure1(:,counter).*Nvect1(:,2)]);
      Fp2(:,counter) = -dS(2)*sum([Pressure2(:,counter).*Nvect2(:,1),Pressure2(:,counter).*Nvect2(:,2)]);
      Fp3(:,counter) = -dS(3)*sum([Pressure3(:,counter).*Nvect3(:,1),Pressure3(:,counter).*Nvect3(:,2)]);
      % viscous
        % radial component of the Un gradient
        dnUn1_xy = sum([dnUn1(:,counter).*Nvectm1(:,1),dnUn1(:,counter).*Nvectm1(:,2)]);
        dnUn2_xy = sum([dnUn2(:,counter).*Nvectm2(:,1),dnUn2(:,counter).*Nvectm2(:,2)]);
        dnUn3_xy = sum([dnUn3(:,counter).*Nvectm3(:,1),dnUn3(:,counter).*Nvectm3(:,2)]);
        % radial component of the Ut gradient
        dnUt1_xy = sum([dnUt1(:,counter).*Tvectm1(:,1),dnUt1(:,counter).*Tvectm1(:,2)]);
        dnUt2_xy = sum([dnUt2(:,counter).*Tvectm2(:,1),dnUt2(:,counter).*Tvectm2(:,2)]);
        dnUt3_xy = sum([dnUt3(:,counter).*Tvectm3(:,1),dnUt3(:,counter).*Tvectm3(:,2)]);
      % All
      Fv1(:,counter) = (1/Re)*dS(1)*(2*dnUn1_xy+dnUt1_xy);
      Fv2(:,counter) = (1/Re)*dS(2)*(2*dnUn2_xy+dnUt2_xy);
      Fv3(:,counter) = (1/Re)*dS(3)*(2*dnUn3_xy+dnUt3_xy);
      counter = counter+1;
   end

%% Drag power
    jc = transpose(Fp3(1,:)+Fp2(1,:)+Fp1(1,:)+Fv3(1,:)+Fv2(1,:)+Fv1(1,:));
    Jc = mean(jc);

%% Reduction fluctuation level in lift
    Lift = Fp3(2,:)+Fp2(2,:)+Fp1(2,:)+Fv3(2,:)+Fv2(2,:)+Fv1(2,:);
    dLift_dt = diff(Lift)/tstep;
    jd = transpose(sqrt(   cumsum(dLift_dt.^2)./((tinit+tstep):tstep:tend)   ));
    Jd = jd(end);

%% Total fluctuation energy
     % Compute Ufluc and Vfluc
    Ub = mean(U(:,floor(NTStp/4):end),2);
    Vb = mean(V(:,floor(NTStp/4):end),2);
    Ufluc = U-Ub;
    Vfluc = V-Vb;
    % Space integration
%     area = load('PinballData/area.dat','-ascii');
    % Ufluc and Vfluc value for each triangle
    Ufs = (Ufluc(elem(:,1),:)+Ufluc(elem(:,2),:)+Ufluc(elem(:,3),:))/3;
    Vfs = (Vfluc(elem(:,1),:)+Vfluc(elem(:,2),:)+Vfluc(elem(:,3),:))/3;
    % TKE
    k = sum((Ufs.^2+Vfs.^2).*area,1);
    je = k';
    Je = mean(je);
%% Recirculation bubble
     bubble_nodes = load('Cost_Data/bubble_nodes.dat','-ascii');
    % Negative velocities (logical)
    Uneg = U(bubble_nodes,:)<0;
    % Retrieve the good x coordinates
    xneg = grid2(bubble_nodes,1);
    xb = xneg.*Uneg;
    yb = grid2(bubble_nodes,2).*Uneg;
    % max x
    [xcirc,idx] = max(xb,[],1); %recirc_bubble_length
    ycirc = NaN(1,size(xb,2));
    for p=1:size(xcirc,2)
        ycirc(p) = yb(idx(p),p);
    end
    jf = [xcirc',ycirc'];
    Jf = mean(jf(floor(NTStp/4):end,1));
    
%% Compute symmetries
            Ns = size(W,1);
            Uy=zeros(NTStp,Ns);
            for p=1:Ns
                    UYSYM = transpose(Uysym(:,(1:3)+3*(p-1)));
                    Uy(:,p)=interpolation(W(p,:),UYSYM);
            end

            jg = mean(Uy.^2,2);
            Jg = mean(jg);

%% Output
    gamma = 1;
    J = Jc+gamma*Jb;   % I change the wake stability to the 
    J_out = {J,Ja,Jb,Jc,Jd,Je,Jf,Jg};
    fprintf(['      Cost of ',name,' is ',num2str(J),'\n']);

    
%% Save
    % quantities variables
    time = tinit:tstep:tend; Time = time';
    Fx = [Time,Fp3(1,:)',Fp2(1,:)',Fp1(1,:)',Fv3(1,:)',Fv2(1,:)',Fv1(1,:)'];
    Fy = [Time,Fp3(2,:)',Fp2(2,:)',Fp1(2,:)',Fv3(2,:)',Fv2(2,:)',Fv1(2,:)'];
    Umt = [Time,Umt3,Umt2,Umt1];
    Torque = [Time,Torque3,Torque2,Torque1];
    
    % save variables
    L2SS = ja;
    ActPower = jb;
    DragPower = jc;
    LiftFluct =  jd;
    RecircBubble = jf;
    TKE = je;
    VSym = jg;

     if do_you_want_to_save
        save(['../../../../PinballMF/OUTPUT/robust/',name,'.mat'], 'Fx','Fy','Torque','Umt',...
            'Time','ActPower','DragPower','L2SS','LiftFluct','RecircBubble','TKE','VSym');
    end
    %% New version to save 
    % if do_you_want_to_save
     %    save(['../Forces_Data/',name,'.mat'], 'Fx','Fy','Torque','Umt',...
      %       'Time','ActPower','DragPower','L2SS','LiftFluct','RecircBubble','TKE','VSym');
    % end
    %% Over it
  
	% Ja : L2 to steady solution
	% Jb : Actuation power
	% Jc : Drag power
	% Jd : Lift fluctuation reduction
	% Je : Total fluctuation energy
	% Jf : Recirculation bubble
	% Jg : V energy
    J_MLC = [Jc,Jb,Ja,Jd,Je,Jf,Jg];

%% Save for MLC3
       save(['../../../../PinballMF/OUTPUT/robust/',name,'.dat'], 'J_MLC','-ascii');
% save(['../Costs/',name,'.dat'], 'J_MLC','-ascii');
