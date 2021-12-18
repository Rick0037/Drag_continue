      subroutine assemble_matrix(NEL,nen,ndf,ndm,MEN,MDF,NOPP, IX,x,xl,
     +ul,NOPPL,MatGroup,Dmat, kbc,VBC,nbcV,IA,JA,A,ITMP,F,F1,F2,
     +F3,F4,
     +RD,VolXg, VolYg,af,rdl,dt, NDIM,NZMAX,NZLMAX,
     +NZUMAX,NDEmax,NELmax,MatMax, MaxProp,MaxCon,LDOF,isteady)
 
 
      implicit none
 
      logical BOUNDARY
 
      integer ndf,nen,ndm,NDIM,NZMAX,NZLMAX,NZUMAX, NDEmax,NELmax, NDE,
     +NEL,NMAX,nbcV, MatMax,MaxProp,MaxCon
 
 
 
 
C     Boundary conditions v
      real*8 VBC(NDEmax,NDF)
      integer KBC(NDEmax,NDF+1)
 
C     Material Properties
      real*8 DMat(MatMax,MaxProp)
      integer MatGroup(NELmax,2)

C     Nodal coordinates
      real*8 x(ndm,ndemax)
 
C     Connectivity matrix
      integer IX(NEN,NELmax)
 
      real*8 F(ndim,5),F1(ndim,5),F2(ndim,5),
     .                 F3(ndim,5),F4(ndim,5),RD(ndim)
      double precision      VolXg(NDEmax), VolYg(NDEmax) 
 
C     Controls for unsteady simulation
      real*8 dt
 
      INTEGER IA(NDIM+1), JA(NZMAX),ITMP(NDIM+1)
      DOUBLE PRECISION A(NZMAX)
 
      integer NOPP(NDEmax),NOPPL(NEN),MDF(NDEmax),MEN(NELmax)
 
 
C     Local values
      real*8  ul(ndf,nen,10), xl(ndm,nen)
 
C     Element matrix (AF), local residuum vector (RDL)
       real*8 AF(NDF*NEN,NDF*NEN), RDL(NDF*NEN)
 
    
      integer i,j,k,l,m,n,iElem, iMat,Lgrid,Lcontrol,Lsolver, Lmaterial,
     +LDOF,NMat,iRest,isw,NCOUNT, KK,JJ,KG,JG,KG0,JG0,KL,JL,isteady,
     +ieig,ipass,NDecompose, NEig
 
 
 
      print*, 'Starting assembling'
   
 
C-----Assembling the Main Matrix -----------------------
 
C     I = element number
 
c----------------------------------------
 
      DO 500   I=1,NEL
c---------------------------------------
         do j=1,nen*ndf
            do k=1,nen*ndf
               af(j,k) = 0.0D0
            enddo
            rdl(j) = 0.0D0
         enddo
         do j=1,ndf
            do k=1,nen
               do l=1,10
                  ul(j,k,l) = 0.0D0
               enddo
            enddo
         enddo
c---------------------------------------
 
C     I = element number
C     MEN(I) - number of nodes for element i
C     ndm - number of spatial dimensions
c      nen ->  MEN
c      ndf ->  MDF
 
         do j=1,MEN(i)
 
c     j = node number (local)
            do l = 1, ndm
               xl(l,j) = x(l,  abs(ix(j,i)) )
            enddo
            do k = 1, MDF(abs(IX(j,I)))
	    
               ul(k,j,1) = F ( NOPP ( abs(IX(j,i)) ) + k - 1, 1)
c               ul(k,j,2) = F ( NOPP ( abs(IX(j,i)) ) + k - 1, 2)
c               ul(k,j,3) = F ( NOPP ( abs(IX(j,i)) ) + k - 1, 3)
c               ul(k,j,4) = F ( NOPP ( abs(IX(j,i)) ) + k - 1, 4)
c               ul(k,j,5) = F ( NOPP ( abs(IX(j,i)) ) + k - 1, 5)
	       
c	       if (F ( NOPP ( abs(IX(j,i)) ) + k - 1, 1)   .ne.0 )        
c     .	       write(*,*) i, IX(j,i), MDF(abs(IX(j,I))),MEN(i),ul(k,j,1) 
		  
            enddo
         enddo
 

C FLOW++++++++++++++++++++++++++



c	 if (MatGroup(I,1).eq.666) then

           if (nen.eq.10) then


            call flow10(I,Dmat,MatGroup(I,2),NOPPL,MEN,MDF,
     +      ul,xl,ix,af,rdl,dt,3, NDIM,
     +      NZMAX,NZLMAX,NZUMAX,NDEmax,NELmax,MatMax, MaxProp,MaxCon,
     +      ndf,nen,ndm,F,F1,F2,F3,F4,RD,VolXg,VolYg,NOPP,isteady)


           else if (nen.eq.6) then


            call flow6(I,Dmat,MatGroup(I,2),NOPPL,MEN,MDF,
     +      ul,xl,ix,af,rdl,dt,3, NDIM,
     +      NZMAX,NZLMAX,NZUMAX,NDEmax,NELmax,MatMax, MaxProp,MaxCon,
     +      ndf,nen,ndm,F,F1,F2,F3,F4,RD,VolXg,VolYg,NOPP,isteady)

           else
  
           stop 'wrong nen'

           endif

c         else
c            stop 'Wrong element group'
c         endif
 
 
         DO 600 K=1,MEN(I)

            KK = abs( IX(K,I) )
            KG0 = NOPP(KK)
 

            DO 600 L=1,MDF(KK)

               KG = KG0 + L - 1
 
               IF (IX(K,I).LT.0) THEN
 
                  CALL CHECKDOF(L,IX(K,I),KBC,nbcV,BOUNDARY,NDEmax,NDF)
 
                  IF(BOUNDARY) THEN
 
                     call skset(KG,KG,1.0D0, A,IA,JA,ITMP,LDOF, NDIM,
     +               NZMAX)
                     go to 600
 
                  END IF
 
               END IF
 
               KL = NOPPL(K) + L - 1
 
               DO 50 J=1,MEN(I)
 
                  JJ = abs( IX(J,I) )
                  JG0 = NOPP(JJ)
 
                  DO 50 M=1,MDF(JJ)
 
                     JG = JG0 + M - 1
 
                     IF (IX(J,I).LT.0) THEN
 
                        CALL CHECKDOF(M,IX(J,I),KBC,nbcV,BOUNDARY,
     +                  NDEmax,NDF)
 
 
                        IF(BOUNDARY) THEN
 
                           call skset(JG,JG,1.0D0, A,IA,JA,ITMP,LDOF,
     +                     NDIM,NZMAX)
 
                           go to 50
 
                        END IF
 
                     END IF
 
                     JL = NOPPL(J) + M - 1
                     if(af(kl,jl).ne.0.0) call skadd(jg,kg,af(kl,jl), A,
     +               IA,JA, ITMP,LDOF, NDIM,NZMAX)
 
 
50             CONTINUE
               RD(KG) = RD (KG) + RDL (KL)
600      CONTINUE
500   CONTINUE
 
C-End ----Assembling the Main Matrix -----------------------
 
      write(*,*) 'Matrix assembled'

 
      return
      end
 
 
 
 
 
 
 
 
