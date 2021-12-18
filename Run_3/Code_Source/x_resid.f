      subroutine resid(NDE,NDEmax,ndim,NCOUNT,RD,NOPP,RMAX,T)
 
 
      implicit none
 
 
      integer NDE,NDEmax,ndim,NCOUNT,NOPP(NDEmax),i
      real*8 RMAX,RESVx,RESVy,T
      real*8 RD(ndim)
 
      RESVx=0
      RESVy=0
 
      DO I=1,NDE
 
         RESVx=MAX( RESVx,ABS( RD(NOPP(I) ) ) )
         RESVy=MAX( RESVy,ABS( RD(NOPP(I)+1) ) )
 
      ENDDO
 
 
      RMAX =MAX(RESVx,RESVy)
      write(*,*) 'RESVx=',RESVx,'  RESVy=',RESVy  
 
      write(*,*)'#################################################'
      write(*,*) 'RMAX=  ', RMAX, '  TIME=',T, '   NCOUNT=', NCOUNT
      write(*,*)'#################################################'
 
      return
      end
 
 
 
 
 
 
 
