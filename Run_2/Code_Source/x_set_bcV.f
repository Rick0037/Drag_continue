      subroutine set_bcV(NDEmax,ndim,ndf,NOPP,MDF,nbcV,nbcF,kbc,vbc,F)
 
      implicit none
 
      integer NDEmax,ndim,ndf,nbcV,nbcF
      integer NOPP(NDEmax), MDF(NDEmax)
      real*8 F(ndim,5)
 
C     Boundary conditions v
      real*8 VBC(NDEmax,NDF)
      integer KBC(NDEmax,NDF+1)
      integer i,j
 
C-End -----Boundary Conditions -I-----for unsteady -------------
      do i=2,ndf+1
         do j=1,nbcV
            IF (KBC(J,I).NE. 0.AND. I- 1.LE. MDF(KBC(J,1)) ) then
               F (NOPP (KBC(J,1) )+I- 2,  1) = VBC(J,I-1)
            END IF
         enddo
      enddo
C-End -----Boundary Conditions -I-----for unsteady -------------
      
      return
      end
 
 
 
 
 
 
 
 
 
