      subroutine set_bc_RD(NDEmax,ndim,ndf, NOPP,MDF,nbcV,nbcF,kbc,vbc,
     +RD)
 
      implicit none
 
      integer NDEmax,ndim,ndf,nbcV,nbcF
      integer NOPP(NDEmax), MDF(NDEmax)
      real*8 RD(ndim)
 
C     Boundary conditions v
      real*8 VBC(NDEmax,NDF)
      integer KBC(NDEmax,NDF+1)
      integer i,j
 
 
      do i=2,ndf+1
         do j=1,nbcV
            IF (KBC(J,I).NE. 0.AND. I- 1.LE. MDF(KBC(J,1)) ) then
               RD (NOPP (KBC(J,1) )+I-2) = 0.0D0
            END IF
         enddo
      enddo
 
      return
      end
 
 
 
 
 
 
 
 
 
