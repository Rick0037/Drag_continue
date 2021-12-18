function []=Vortmove(Ntint,Ntend)
%%input the movie
step=0.1;
Nstart=Ntint/step;
Nend=Ntend/step;
nstep=Nend-Nstart;
% framesPath = '/home/dyfluid/111/Run_1/MatlabV3/';

object=VideoWriter('flow.avi');
object.FrameRate=1/step;
open(object);

% M(nstep) = struct('cdata',[],'colormap',[]);

grid=load('./Grid2.dat','-ascii');
elem=load('./elem.dat','-ascii'); 
X = grid(:,1);
Y = grid(:,2);

vortmin = -1.5;  % only plot what is in -0.1 to 0.1 range
vortmax = 1.5;
%% Fitst picture



figure(1)
set(gca,'XLim',[-6 20]);
set(gca,'YLim',[-6 6]);

for p = 1:nstep

N_snapshot=p+Nstart-1;
filename=sprintf('%s%06d','../Code_Output/Flow.',N_snapshot);
Flow = load(filename,'-ascii');       
U=Flow(1:8633,1);
V=Flow(1:8633,2);

VORT  = Comp_Vorticity(U,V,grid,elem);

%% Visulalization
% figure(1)
% set(gca,'Position',[0.07 0.08 0.7 0.8]);
% set(gca,'XLim',[-5 20]);
% set(gca,'YLim',[-6 6]);
VORT(VORT>vortmax) = vortmax;  % cutoff at vortmax
VORT(VORT<vortmin) = vortmin;  % cutoff at vortmin
% hold on
h = trisurf (elem, X, Y, VORT, 'facecolor','interp','EdgeColor','none') ;
% set(gca,'DataAspectRatio',[1 1 1]);
% set(gcf,'Position',[500 519 570 413]);
view(2) ;
axis tight; 
% colorbar;
% caxis([vortmin vortmax])
% caxis([-1.5 1.5])
shading interp 
%
pic_name=sprintf('%s%06d%s','Flow.',N_snapshot,'.png');
saveas(1,pic_name)

% M=imread([framesPath,pic_name]);
J=getframe(gca,[-18.2777780387834,-22.2777781190928,468.527778209441,367.372778289750]);
% close
writeVideo(object,J)
end

close(object);

end