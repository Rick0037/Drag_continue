      subroutine read_mesh(Lgrid, TITLE,NDE,NDEmax,x,ndm, ndf,nen,IX,
     +NEL,NELmax,MatGroup,MDF,MEN, nbcV,nbcF,kbc,vbc,lbc,fbc,XX,YY)


      implicit none

      character*80 InputLine, TITLE

      integer ndf,nen,ndm, NDEmax,NELmax, NDE,NEL,nbcV,nbcF,NMAX,
     +MatMax,MaxProp,MaxCon,MaxEig

      integer i,j,k,l,m,n,iElem, iMat,Lgrid,Lcontrol,Lsolver, Lmaterial,
     +LDOF,NMat,iRest,isw,NCOUNT, KK,JJ,KG,JG,KG0,JG0,KL,JL,isteady,
     +ieig,ipass,NDecompose, NEig

C     Nodal coordinates
      real*8 x(ndm,ndemax)
      double precision XX(NDEmax),YY(NDEmax)

      integer MDF(NDEmax),MEN(NELmax)

C     Connectivity matrix
      integer IX(NEN,NELmax)

C     Boundary conditions v
      real*8 VBC(NDEmax,NDF)
      integer KBC(NDEmax,NDF+1)

C     Boundary conditions F
      real*8 FBC(NDEmax,NDF)
      integer LBC(NDEmax,NDF+1)

C     Material Properties
      integer MatGroup(NELmax,2)

      READ(Lgrid,'(A)') TITLE
c      READ(Lgrid,*)
      READ(Lgrid,*) NDE
      if(NDE.gt.NDEmax) stop 'NDEmax too small'
      do i=1,NDE
c         READ(Lgrid,*) n, (x(j,n),j=1,ndm)
         n=i
         READ(Lgrid,*) (x(j,n),j=1,ndm)
         XX(i)=x(1,i)
         YY(i)=x(2,i)
c         write(*,*)  XX(i), YY(i)
C     for historical reasons
      enddo
      WRITE(*,*) NDE,' nodes read for ' , TITLE
      READ(Lgrid,*)
      READ(Lgrid,*) NEL
      if(NEL.gt.NELmax) stop 'NELmax too small'



c      READ(Lgrid,*)

      do i=1,NEL

         Read (Lgrid, '(A80)' ) InputLine
c         Read(Unit=InputLine, FMT=* ) n, iElem, iMat
c         MatGroup(i,1) = iElem
c         MatGroup(i,2) = iMat

c         if(MatGroup(I,1).eq.666) then
            if(nen. eq. 10) then
            MEN(i) = 10             !  TRIANGLE10
            else 
            MEN(i) = 6             !  TRIANGLE10
            endif

       Read(Unit=InputLine, FMT=* ) (IX(k,i),k=1,MEN
     +      (i))
c            Read(Unit=InputLine, FMT=* ) n, iElem,
c     +                                  iMat,(IX(k,i),k=1,MEN
c     +      (i))
            do j=1,MEN(i)

               if(j.le.3) MDF(IX(j,i)) = 3
               if(j.gt.3) MDF(IX(j,i)) = 2

            end do


c         else
c            stop 'Wrong element applied'
c         endif
      enddo
      WRITE(*,*) NEL,' elements read '
      close(Lgrid)
      return
      end







