      subroutine read_material( NMat,MaxProp,MatMax,DMat, Lmaterial,
     +Fmaterial,Re,eps,Bv)
 
      implicit none
 
      integer NMat,MatMax,MaxProp,MaxCon,MaxEig,Lmaterial
      character*80 Fmaterial
      integer i,j,n
 
C      Material Properties
      real*8 DMat(MatMax,MaxProp),Re,eps,Bv
  
     
      OPEN(UNIT=Lmaterial,FILE=Fmaterial,STATUS='OLD')
      read(Lmaterial,*)
c      read(Lmaterial,*) NMat
      NMat=1
c      MaxProp=3
      do i = 1,NMat
         read(Lmaterial,*)
c         read(Lmaterial,*) n, (DMat(n,j),j=1,MaxProp)
       n=1
       read(Lmaterial,*) (DMat(n,j),j=1,3)
       
        Re=DMat(n,1)
        eps=DMat(n,2)
        Bv=DMat(n,3)
   
         if(n.gt.MatMax)then
            write(*,*) 'Number of the material too high ',n , 'MaxMat=',
     +      MatMax
            stop
         endif
c              write(*,*)i,n, (DMat(n,j),j=1,MaxProp)
      enddo
      close (Lmaterial)
      return
      end
 
 
 
 
 
 
 
