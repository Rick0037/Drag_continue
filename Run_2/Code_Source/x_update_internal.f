      subroutine update_internal(LDOF,ndim,F,RD)
 
      implicit none
 
      integer i,LDOF,ndim
      real*8 F(ndim,5),RD(ndim)
 
 
      do I=1,LDOF
         F(I,1) = F(I,1) +  RD(I)
      enddo
 
      return
      end
 
 
 
 
 
 
 
 
