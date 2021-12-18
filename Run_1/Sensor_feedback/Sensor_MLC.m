function Sensor_MLC(Re,T)
%% save the drag lift lift(T-t) coefficent 
global coefficent
   tdelay=2.2;
   delay=10*tdelay;
   C=0;
 %% Parameters
    direc = ['Code_Output/'];
    NT=10*T;
    NTStp = 1; 
    
%% Load data to compute forces 
    load 'Sensor_feedback/Cost_Data/Mesh_Data'

%% Load Run and compute norm
    % Allocation
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

    % Without Loop
        counter = 1;
        
            if NT<=9
                id1 = '00000';
            elseif NT<=99
                id1 = '0000';
            elseif NT<=999
                id1 = '000';
            elseif NT<=9999
                id1 = '00';
            elseif NT<=99999
                id1 = '0';
            end

        % index 2
            id2 = num2str(NT); % 
        % load
            flow = load([direc,'Flow.',id1,id2]);    
            
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
        
              
%% Actuation power
% Scalar product for each node
    % Initialization
    Ust1 = zeros(length(s1),NTStp); Usn1 = zeros(length(s1),NTStp);
    Ust2 = zeros(length(s2),NTStp); Usn2 = zeros(length(s1),NTStp);
    Ust3 = zeros(length(s3),NTStp); Usn3 = zeros(length(s1),NTStp);

    % Scalar product for each node
    Ust1(:,:) = sum(Us1.*Tvectm1'); % numbers with numbers 
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
   

%% Drag coefficent
    Drag = transpose(Fp3(1,:)+Fp2(1,:)+Fp1(1,:)+Fv3(1,:)+Fv2(1,:)+Fv1(1,:)); % transpot the matrix
    Dragcoefficent=2*Drag;
    Dragnose=Dragcoefficent+Dragcoefficent*(rand*2-1)*C;
    
%% Lift coefficent
    Lift = Fp3(2,:)+Fp2(2,:)+Fp1(2,:)+Fv3(2,:)+Fv2(2,:)+Fv1(2,:);
    Liftcoefficent=2*Lift;
    Liftnose=Liftcoefficent+Liftcoefficent*(rand*2-1)*C;
    
%% Time delay part
    coefficent=load('Sensor_feedback/Coe_sensors.dat','-ascii');
     % coefficent(23:5000,4)=coefficent(1:4978,3);           %to remove the lift  delft is close this line
 %% Those line are in test
     Timepart=length(coefficent);
     Timedelay=Timepart-delay;
     Liftdelay=coefficent(Timedelay,3);
     Delaynose=Liftdelay+Liftdelay*(rand*2-1)*C;
        
     coe=[T,Dragcoefficent,Liftcoefficent,Liftdelay,Dragnose,Liftnose,Delaynose];
%      coe=[T,Dragcoefficent,Liftcoefficent,Liftdelay];
     Time=coefficent(:,1);
%% Save
     % coe=[T,Dragcoefficent,Liftcoefficent]; % while change and add time delay part in further the secound part is Drag the third part is lift
     if ismembertol(T,Time) 
     else
     coefficent = vertcat(coefficent,coe);
    
     save('Sensor_feedback/Coe_sensors.dat','coefficent','-ascii');
     end
   
end
