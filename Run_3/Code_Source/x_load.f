      subroutine load(T,nbcF,lbc,fbc,NOPP,MDF,RD,NDEmax,
     .                NDF,NDIM,isteady)
  
      Implicit none    
c     Implicit DOUBLE PRECISION (A-H, O-Z)

      integer ndf,nen,ndm,NDIM,NZMAX,NZLMAX,NZUMAX, 
     .           NDEmax,NELmax,    
     .           NDE,NEL,nbcV,nbcF,NMAX,  
     .           MatMax,MaxProp,MaxCon
   
      real*8 FBC(NDEmax,NDF),RD(ndim)
      integer LBC(NDEmax,NDF+1), NOPP(NDEmax)
      integer MDF(NDEmax)
      integer i,j,k,l,m,n,isteady
     
      real*8 DT,T0,T
   
      
      
      return
      
      
      
      
      
      
c         IF    ((T.ge. 0.01d0  .and. T .le. 0.1).or.
c     .         (T.ge. 2.5d0  .and. T .le. 2.6).or.
c     .         (T.ge. 5.0d0  .and. T .le. 5.1).or.
c     . 	       (T.ge. 7.5d0  .and. T .le. 7.6)) then 
      
       IF    (T.ge. 0.01d0  .and. T .le. 0.1
     .               .or. isteady.eq.1) then

       do  i=2,ndf+1   
       do  j=1,nbcF

       IF (LBC(J,I).NE.0 .AND. I-1 .LE. MDF(LBC(J,1))) then
              RD (NOPP (LBC(J,1) )+I-2 )  = 
     .        RD (NOPP (LBC(J,1) )+I-2 ) + FBC(J,I-1) 
       write(*,*)'Force ', RD (NOPP (LBC(J,1) )+I-2  ),
     .             FBC(J,I-1) 
       END IF
      enddo   
      enddo

C-End -----Boundary Conditions -I-----for unsteady ------------

      ENDIF

      return
      end
 
