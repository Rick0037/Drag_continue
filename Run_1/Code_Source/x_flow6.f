      subroutine flow6(II,Dmat,Mat,NOPPL,MEN,MDF,
     . ul,xl,ix,af,rdl,dt,isw,
     .        NDIM,NZMAX,NZLMAX,NZUMAX,NDEmax,NELmax,MatMax,
     .        MaxProp,MaxCon,ndf,nen,ndm,F,F1,F2,F3,F4,RD,
     .        VolXg,VolYg,NOPP,isteady)

      
      Implicit none


      integer k,n,m

      integer II,Mat,isw,NDIM,NZMAX,NZLMAX,NZUMAX,NDEmax,NELmax,
     . MatMax,MaxProp,MaxCon,ndf,nen,ndm,isteady


      integer NOPP(NDEmax),NOPPL(NEN),MDF(NDEmax),MEN(NELmax),
     .          IX(nen,NELmax)

      DOUBLE PRECISION AF(NDF*NEN,NDF*NEN),
     .          RDL(NDF*NEN)

       integer KBC(NDEmax,NDF+1)

       DOUBLE PRECISION DMat(MatMax,MaxProp)
       integer MatGroup(NELmax,2)

      DOUBLE PRECISION AE,B1,B2,B3,B4,C1,C2,C3,C4,
     .D1,D2,D3,D4,dt


      DOUBLE PRECISION    xl(ndm,nen),ul(ndf,nen,10)

       DOUBLE PRECISION Re,eps,Bv
       DOUBLE PRECISION x(4),y(4),z(4),d(30,30),fv(30,30)
c       DOUBLE PRECISION Vx(10),Vy(10),Vz(10)
       integer flag,i,j,numdof


       DOUBLE PRECISION A2
        
      double precision F(ndim,5),F1(ndim,5),F2(ndim,5), 
     .                 F3(ndim,5),F4(ndim,5),RD(ndim)
 
          double precision XX(NDEmax),YY(NDEmax),
     .          VBCo(NDEmax,NDF),
     .       vx(nen),vy(nen),p(nen),vx1(nen),vy1(nen),vx2(nen),vy2(nen),
     .          vx3(nen),vy3(nen),vx4(nen),vy4(nen),
     .          vxl(nen),vyl(nen),vxo(nen),vyo(nen),VolX(nen),VolY(nen)
          double precision      VolXg(NDEmax), VolYg(NDEmax)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
      
            
c   local NOPP


* +++ local NOPP

      NOPPL(1)=1
      NOPPL(2)=4
      NOPPL(3)=7
      NOPPL(4)=10
      NOPPL(5)=12
      NOPPL(6)=14

* --- end of local NOPP


c end local NOPP

      Mat=1

       Re=	Dmat(Mat,1)
       eps=     Dmat(Mat,2)
       Bv=	Dmat(Mat,3)
       
       
C     UNSTEADY ------------------------------------        

       numdof=15    
   

!27.03.11            do j=1,MEN(ii)

c     j = node number (local)

!              Vx(j) =  ul(1,j,1)
!              Vy(j) =  ul(2,j,1)

!         enddo

       do j=1,3
          x(j)=xl(1,j)
          y(j)=xl(2,j)
c          z(j)=xl(3,j)    
       enddo
     
            
       A2=(x(2)*y(3) +x(3)*y(1) +x(1)*y(2)
     .   -x(2)*y(1) -x(3)*y(2) -x(1)*y(3))


      AE = A2/2.0

      DO  J=1,MEN(ii)

       VX(J)  = F  ( NOPP ( abs( IX(j,ii)) )                   ,1    )
       VY(J)  = F  ( NOPP ( abs( IX(j,ii)) ) +1             ,1    )  
       if(J.LE.3) P(J) = F ( NOPP ( abs( IX(j,ii)) ) +2         ,1  )  

       if (isteady.eq.0) then
       
       VX1(J) = F1 ( NOPP ( abs( IX(j,ii)) )                 ,1    )
       VX2(J) = F2 ( NOPP ( abs( IX(j,ii)) )                  ,1   )
       VX3(J) = F3 ( NOPP ( abs( IX(j,ii)) )                  ,1   )
       VX4(J) = F4 ( NOPP ( abs( IX(j,ii)) )                  ,1   )

       VY1(J) = F1 ( NOPP ( abs( IX(j,ii)) ) +1           ,1    )
       VY2(J) = F2 ( NOPP ( abs( IX(j,ii)) ) +1            ,1    )
       VY3(J) = F3 ( NOPP ( abs( IX(j,ii)) ) +1            ,1    )
       VY4(J) = F4 ( NOPP ( abs( IX(j,ii)) ) +1            ,1    )
  
       VolX(J) = VolXg(            ( abs( IX(j,ii)) )    )
       VolY(J) = VolYg(            ( abs( IX(j,ii)) )    )
       endif

       END DO


      B1 = ( y(2) - y(3) ) / A2
      B2 = ( y(3) - y(1) ) / A2
      B3 = ( y(1) - y(2) ) / A2

      C1 = ( x(3) - x(2) ) / A2
      C2 = ( x(1) - x(3) ) / A2
      C3 = ( x(2) - x(1) ) / A2
      
c      write(*,*)ii,AE,B1,B2,B3,C1,C2,C3
       

       if (isteady.eq.0) then
      CALL flow_tria_uns6(re,ae,b1,b2,b3,c1,c2,c3,
     .  af,ndf,nen,vx,vy,p,vx1,vy1,vx2,vy2,vx3,vy3,vx4,vy4,
     .    rdl,eps,dt,
     .    vxl,vyl,vxo,vyo,volx, voly, bv)
       else 
      
     
       CALL flow_tria_ste6(RE,AE,B1,B2,B3,C1,C2,C3,
     .            AF,NDF,NEN,VX,VY,P,RDL,eps,Bv)
  
       endif



      do k=1,numdof
         do j=1,numdof
       
       d(k,j)=AF(k,j)
       

         enddo
	 
	 rdl(k)=-rdl(k)
	 
	 
      enddo

      return
      
      end



     






