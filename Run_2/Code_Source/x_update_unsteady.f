      subroutine update_unsteady(ndim,LDOF,F,F1,F2,F3,F4,dt)
 
      implicit none
 
      integer ndim,LDOF,i,j
      real*8 F(ndim,5),F1(ndim,5),F2(ndim,5),
     .              F3(ndim,5),F4(ndim,5),dt
 
 
C- update - unsteady-------------------------------------
      do I=1,LDOF
         do j=1,5
            F4(I,j)= F3(I,j)
            F3(I,j)= F2(I,j)
            F2(I,j)= F1(I,j)
            F1(I,j)= F (I,j)
         enddo
      enddo
C- update - unsteady-------------------------------------
 
      return
      end
 
 
 
 
 
 
 
 
