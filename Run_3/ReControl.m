function ReControl(Re)
T=0;
system ('rm -f Control_Barrier');

addpath('../MLC/MLC_tools');
addpath('Sensor_feedback/');
more off;
%***** Loop over time **************************************************
%***** Here is some T time set as the limit, it can be endless loo too.
while T < 750


if (exist('Covergent_Barrier', 'file') == 2 )
    system ('rm -f Control_Barrier');
    break;
% else
end
       
    % MATLAB has no "UNTIL" loop !!!
% Wait till Flow Solver writes the data and sets Control_Barrier
    while(1)
        if (exist('Control_Barrier', 'file') == 2 || exist('Covergent_Barrier', 'file') == 2)
            break;
        end;
    end;

% Alow output to stdout (presently only time T is written)
more off;


% Read Fortran written files - watch markers written by "SEQUENTIAL"  
 fid=fclose('all');
 fid=fopen('Control_Input.dat','rb');
 fseek(fid, 4, 'cof');

T   =fread(fid, 1, 'real*8')
DT  =fread(fid, 1, 'real*8');
NDE =fread(fid, 1, 'int32');
NEL =fread(fid, 1, 'int32');
NDEmax=fread(fid, 1, 'int32');
NELmax=fread(fid, 1, 'int32');
III   =fread(fid, 6*NELmax, 'int32');
IX     =reshape(III,[6,NELmax]);
NOPP   =fread(fid,NDEmax,'int32');
XX     =fread(fid,NDEmax,'real*8');
YY     =fread(fid,NDEmax,'real*8');
VolXg  =fread(fid,NDEmax,'real*8');
VolYg  =fread(fid,NDEmax,'real*8'); 
Nvf    =fread(fid, 1, 'int32');
NBC    =fread(fid, 1, 'int32');
III    =fread(fid, NDEmax*4, 'int32');
KBC    =reshape(III,[NDEmax,4]);
FFF    =fread(fid,NDEmax*3,'real*8');
VBCo   =reshape(FFF,[NDEmax,3]);
FFF    =fread(fid,NDEmax*3,'real*8');
VBC    =reshape(FFF,[NDEmax,3]);
FFF    =fread(fid,NDEmax*2*5,'real*8');
F      =reshape(FFF,[NDEmax*2,5]);
PRESS  =fread(fid,NDEmax,'real*8'); 

U1 = 0.0; % ... for the cylinder at the front
U2 = 0.0; % ... for the cylinder at the back, bottom
U3 = 0.0; % ... for the cylinder at the back, top
%%***** Actuation extracted from LGP/Populations and generation gen 
% MLC
gen = 10;
ind = 5;

PHI = (sqrt(5)+1)/2;
% MLC
LOAD_DATA = load(['../MLC/save_runs/Pinball47/Populations/Gen',num2str(gen),'population.mat']);
GenPopulation = LOAD_DATA.GenPopulation;

%%***** Sensor ********************************************************
global coefficent
Sensor_MLC(Re,T-0.1)

coefficent=load('Sensor_feedback/Coe_sensors.dat','-ascii');
eval(['U1 = ',GenPopulation{ind,1},';']);
eval(['U2 = ',GenPopulation{ind,2},';']);
eval(['U3 = ',GenPopulation{ind,3},';']);

%***** Implement control law in boundary nodes *************************
if  T==0.1
% printf ("first step\n");

elseif T>=0.2
%***** First cylinder, front
%***** R=0.5, (x_0,y_0) = (-1.5 * sqrt(3./4.) , 0),
       for j = 379:522  % 96 nodes on a circle
          i=KBC(j,1) ;  % node number
          VBC(j,1) = -2 * U1 * (YY(i) - 0)             ; % V_x    
          VBC(j,2) =  2 * U1 * (XX(i) + sqrt(27./16.)) ; % V_y
       end

%***** Second cylinder, bottom right
%***** R=0.5,  (x_0,y_0) = (0, -3/4), 
       for j = 235:378 % 96 nodes on a circle
          i=KBC(j,1);   % node number
          VBC(j,1) = -2 * U2 * (YY(i) + 0.75) ; % V_x 
          VBC(j,2) =  2 * U2 * (XX(i) - 0.00) ; % V_y
       end

%***** Third cylinder, top right
%***** R=0.5,  (x_3,y_3) = (0, 3/4)
       for j = 1:144 % 96 nodes on a circle
           i = KBC(j,1);
          VBC(j,1) = -2 * U3 * (YY(i) - 0.75) ; % V_x    
          VBC(j,2) =  2 * U3 * (XX(i) - 0.00) ; % V_y
       end
%***** End of "elseif T>=0.3" 
end



% Transfer of actuation (Dirichlet BC and Volume Forces) to the flow solver
% Note, Fortran MUST read it with access="STREAM" !!!

fid = fopen('Control_Output.dat','w');
fwrite(fid,VBC,'real*8');
fwrite(fid,VolXg,'real*8');
fwrite(fid,VolYg,'real*8');
fid= fclose('all');

% Remove Control_Barrier set by Flow Solver and let CFD computation go

system ('rm -f Control_Barrier');

end
end
