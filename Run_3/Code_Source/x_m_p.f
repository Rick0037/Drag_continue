c     program vx,vy,p
 
      SUBROUTINE PRESSURE(NDE,NEL,IX,XX, YY,F_V,Ndim_v,F,
     .                       NOPP,MDF)


      Implicit DOUBLE PRECISION (A-H, O-Z)

      include 'ndf.inc'

      include 'parameter.inc'

      common /rest/ ldof

      DOUBLE PRECISION F_V(Ndim_v,5)



      DIMENSION F(NDEmax)

      DIMENSION XX(NDEmax),YY(NDEmax),
     .          NOPP(NDEmax),MDF(NDEmax),
     .          IX(NEN,NELmax)

C================================================================
     
      IF(nen .eq. 10) then  ! qubic

      do  I=1,NEL

      F(abs(IX(1,I)))=F_V(NOPP(abs(IX(1,I)))+2,1)
      F(abs(IX(2,I)))=F_V(NOPP(abs(IX(2,I)))+2,1)
      F(abs(IX(3,I)))=F_V(NOPP(abs(IX(3,I)))+2,1)
      F(abs(IX(4,I)))=(2./3.*F(abs(IX(1,I)))+1./3.*F(abs(IX(2,I))))
      F(abs(IX(5,I)))=(1./3.*F(abs(IX(1,I)))+2./3.*F(abs(IX(2,I))))
      F(abs(IX(6,I)))=(2./3.*F(abs(IX(2,I)))+1./3.*F(abs(IX(3,I))))
      F(abs(IX(7,I)))=(1./3.*F(abs(IX(2,I)))+2./3.*F(abs(IX(3,I))))
      F(abs(IX(8,I)))=(1./3.*F(abs(IX(1,I)))+2./3.*F(abs(IX(3,I))))
      F(abs(IX(9,I)))=(2./3.*F(abs(IX(1,I)))+1./3.*F(abs(IX(3,I))))
      F(abs(IX(10,I)))=(1./3.*F(abs(IX(1,I)))+1./3.*F(abs(IX(2,I)))
     .       + 1./3.*F(abs(IX(3,I))) )


      enddo

      else    ! quadratic

      do  I=1,NEL

      F(abs(IX(1,I)))=F_V(NOPP(abs(IX(1,I)))+2,1)
      F(abs(IX(2,I)))=F_V(NOPP(abs(IX(2,I)))+2,1)
      F(abs(IX(3,I)))=F_V(NOPP(abs(IX(3,I)))+2,1)
      F(abs(IX(4,I)))=(F(abs(IX(1,I)))+F(abs(IX(2,I))))/2
      F(abs(IX(5,I)))=(F(abs(IX(2,I)))+F(abs(IX(3,I))))/2
      F(abs(IX(6,I)))=(F(abs(IX(3,I)))+F(abs(IX(1,I))))/2

      enddo
      
      endif

C------------------------------------------------------------------
      return


      END  

