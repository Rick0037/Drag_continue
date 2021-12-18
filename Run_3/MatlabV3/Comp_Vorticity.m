function [VORTALL] = Comp_Vorticity(UALL,VALL,grid,CONN)
% This function is used to solve the vorticity field from the velocity
% field of Fluidic Pinball. \
% The mesh information is 8633 nodes and 4225 TRIA elements.

Nt=size(UALL,2); %time step
NDE = 8633; NEL = 4225;

X=grid(:,1);
Y=grid(:,2);

% calculate vorticity field
VORTALL = zeros (NDE,Nt);

for i=1:Nt
    U=UALL(:,i);
    V=VALL(:,i);

    DX=0 ;
    DY=0 ;
%     VORTmax=0 ;
%     VORTmin=0 ;
    VORT = zeros (NDE,1);
    IND = zeros (NDE,1);
    
    for KC=1:4*NEL
        K1= CONN(KC,1);
        K2= CONN(KC,2);
        K3= CONN(KC,3);
        K4= CONN(KC,1);
        
        X1 = X(K1);
        X2 = X(K2);
        X3 = X(K3);
        X4 = X(K4);
        DX = max(DX, max([X1,X2,X3,X4])-min([X1;X2;X3;X4]));
        Y1 = Y(K1);
        Y2 = Y(K2);
        Y3 = Y(K3);
        Y4 = Y(K4);
        DY = max(DY, max([Y1;Y2;Y3;Y4])-min([Y1;Y2;Y3;Y4]));
        U1 = U(K1);
        V1 = V(K1);
        U2 = U(K2);
        V2 = V(K2);
        U3 = U(K3);
        V3 = V(K3);
        U4 = U(K4);
        V4 = V(K4);

        AREA = ( (X3-X1)*(Y4-Y2) + (X2-X4)*(Y3-Y1) ) ;
        CIRC = ( (U1+U2)*(X2-X1) + (V1+V2)*(Y2-Y1)...
            + (U2+U3)*(X3-X2) + (V2+V3)*(Y3-Y2)...
            + (U3+U4)*(X4-X3) + (V3+V4)*(Y4-Y3)...
            + (U4+U1)*(X1-X4) + (V4+V1)*(Y1-Y4) ) / AREA ;

        VORT(K1) = VORT(K1) + CIRC ;
        IND(K1) = IND(K1) + 1;
        VORT(K2) = VORT(K2) + CIRC ;
        IND(K2) = IND(K2) + 1;
        VORT(K3) = VORT(K3) + CIRC ;       
        IND(K3) = IND(K3) + 1;
        if K4 ~= K1 
            VORT(K4) = VORT(K4) + CIRC ;
            IND(K4) = IND(K4) + 1;
        end
% % Vort_min and Vort_max
%         if VORTmax < VORT(K1) 
%             VORTmax = VORT(K1) ;
%         end
% 
%         if VORTmax < VORT(K2) 
%             VORTmax = VORT(K2) ;
%         end
% 
%         if VORTmax < VORT(K3) 
%             VORTmax = VORT(K3) ;
%         end
% 
%        
%         if VORTmin > VORT(K1) 
%             VORTmin = VORT(K1) ;
%         end
% 
%        if VORTmin > VORT(K2)
%            VORTmin = VORT(K2) ;
%        end
% 
%        if VORTmin > VORT(K3) 
%            VORTmin = VORT(K3) ;
%        end
    end
    VORTALL(:,i)=VORT./IND;
end

end

