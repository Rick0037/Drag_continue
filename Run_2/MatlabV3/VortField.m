function [] = VortField(N_snapshot)
%% This scripts is used for Visualization 
% Last modidield by Nan DENG at 10:53, 10 Nov. 2020 

% N_snapshot: Number of snapshots

%% ----- Input data -----
filename=sprintf('%s%06d','../Code_Output/Flow.',N_snapshot);
Flow = load(filename,'-ascii');       
U=Flow(1:8633,1);
V=Flow(1:8633,2);
%% 
grid=load('./Grid2.dat','-ascii');
elem=load('./elem.dat','-ascii'); 
VORT  = Comp_Vorticity(U,V,grid,elem);

%% Visulalization
X = grid(:,1);
Y = grid(:,2);
vortmin = -1.5;  % only plot what is in -0.1 to 0.1 range
vortmax = 1.5;


figure(1)
VORT(VORT>vortmax) = vortmax;  % cutoff at vortmax
VORT(VORT<vortmin) = vortmin;  % cutoff at vortmin
hold on
h = trisurf (elem, X, Y, VORT, 'facecolor','interp','EdgeColor','none') ;
set(gca,'DataAspectRatio',[1 1 1]);
set(gca,'FontName','Times New Roman','FontSize',16);
view(2) ;
axis tight; 
colorbar;
caxis([-1.5 1.5])
shading interp 
%
pic_name=sprintf('%s%06d%s','Flow.',N_snapshot,'.png');
saveas(1,pic_name)



