c NEN - Number of nodes in the element (max)
c NDF - Number of Degrees of Freedom in node(max)
c NDIM - Reserved number of dimensions
c NZMAX - Number of Nonzero elements in global matrix
c NDEmax - Reserved number of nodes
c NELmax - Reserved number of elements
c NDM - Number of spatial dimensions
c F(ndim,5) - solution at time k
c F1(ndim,5) - solution at time k-1
c F2(ndim,5) - solution at time k-2 ....etc.
c F*(ndim,1) - u (velocity)
c F*(ndim,4) - not used
c F*(ndim,5) - not used
c RD(NDIM) residuum vector, also used  for increment after solution of linear  system
c RDL local (on element level)  residuum vector
c NOPP(NDEmax) points to first DOF of a node
c NOPPL(NEN)  points to first DOF of a node locally
c MDF(NDEmax)  number of DOFs for the node
c MEN(NELmax) number of nodes per element
c nbcV number of displacment boundary conditions ( applied to nodes )
c nbcF number of force boundary conditions ( applied to nodes )
c        ul(ndf,nen,10)local values of F for degree of freedom, node,
c       - reserved 10 values, in use 1 !
c        xl(ndm,nen) - local values of coordinates for dimensions x,y,z and node



      PROGRAM Unsteady

      Implicit none

      integer ndf,nen,ndm,NZMAX,NZLMAX,NZUMAX, NDEmax,NDIM,NELmax, NDE,
     +NEL,nbcV,nbcF,NMAX,MatMax,MaxProp,MaxCon,MaxEig,NvfP,Nvf


C     Include parameters - here the size of the problem is defined

      include 'ndf.inc'
      include 'parameter.inc'

C     Controls for unsteady simulation

      double precision shift,REVMax

C     Local values
      double precision  ul(ndf,nen,10), xl(ndm,nen)

C     Values of computed vectors for t, t-1, t-2, ... t-n and residuum


      double precision F(ndim,5),F1(ndim,5),F2(ndim,5), 
     .                 F3(ndim,5),F4(ndim,5),RD(ndim)



C     Variables used by the solver
      INTEGER IA(NDIM+1), JA(NZMAX),ITMP(NDIM+1)
      DOUBLE PRECISION A(NZMAX)

C     Variables for the data format and the preconditioners.
      INTEGER MAXN, MAXPQ, MAXVW
      PARAMETER (MAXN = 5000, MAXPQ = 3, MAXVW = 3)
      LOGICAL JT(NDIM)
      INTEGER IDA(NDIM), ILMAT(NDIM+1), IU(NDIM+1)
      INTEGER JI(NDIM), JLMAT(NZLMAX), JU(NZUMAX)
      DOUBLE PRECISION AINV(NDIM), DR(NDIM)
      DOUBLE PRECISION LMAT(NZLMAX), UMAT(NZUMAX)
      DOUBLE PRECISION DN(NDIM), DS(NDIM)
      DOUBLE PRECISION VECS(NDIM,5*(MAXPQ+MAXVW)+4), XZR(NDIM)

C     Variables used by the eigenvalue solver
c      INTEGER    IB (NDIM+1),JB (NZMAX)
c      DOUBLE PRECISION B(NZMAX)
c      double precision R (NDIM,MaxEig)
c      double precision Q (NDIM,MaxEig)
c      double precision C (NDIM,MaxEig)
c      double precision V (NDIM,MaxEig)
c      double precision D (NDIM)
c      double precision XD(NDIM)


C     Nodal coordinates
      double precision x(ndm,ndemax)

C     NOPP points to first DOF of a node (NOPPL - the same locally),
C     MDF - number of DOFs for the node
C     MEN number of nodes per element

      integer NOPP(NDEmax),NOPPL(NEN),MDF(NDEmax),MEN(NELmax)

C     Connectivity matrix
      integer IX(NEN,NELmax)

C     Nodes neighbour's
      integer Neighbour(NDEmax,MaxCon)

C     Element matrix (AF), local residuum vector (RDL)
      double precision AF(NDF*NEN,NDF*NEN), RDL(NDF*NEN)

C     Boundary conditions v
      double precision VBC(NDEmax,NDF)
      integer KBC(NDEmax,NDF+1)

C     Boundary conditions F
      double precision FBC(NDEmax,NDF)
      integer LBC(NDEmax,NDF+1)

C     Material Properties
      double precision DMat(MatMax,MaxProp)
      integer MatGroup(NELmax,2)


      LOGICAL EX,BOUNDARY,NEWMAT
      character*80   Fgrid, Fboundary,Fcontrol, Fsolver, Fmaterial,
     +TITLE

c23456789012345678901234567890123456789012345678901234567890123456789012
c               10                  20                  30                 40                   50                 60                  70
      integer i,j,k,l,m,n,iElem, iMat,Lgrid,Lboundary,Lcontrol,Lsolver,
     +Lmaterial,LDOF,NMat,iRest,isw,NCOUNT, KK,JJ,KG,JG,KG0,JG0,
     +KL,JL,isteady,ieig,ipass,NDecompose, NEig

      double precision DT,T0,T,TMAX,RRHSMAX,PlotTime, RMAX

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      double precision XX(NDEmax),YY(NDEmax),
     .          VBCo(NDEmax,NDF),
     .          vx(nen),vy(nen),vx1(nen),vy1(nen),vx2(nen),vy2(nen),
     .          vxl(nen),vyl(nen),vxo(nen),vyo(nen),VolX(nen),VolY(nen)
      double precision      VolXg(NDEmax), VolYg(NDEmax)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      double precision Re,eps,Bv

      double precision PRESS(NDEmax)
ccc      common / pressure_field / PRESS ! <<< BRN040516 ! via parameter MM 050622

*     LEO 050413
*     if we start from zero or from steady solution, then enable the
*     kick to unsteadiness (.true.), if unsteady restart is loaded then
*     no kick (.false.)
      logical kickMe


* +++ initializations

c      kickMe = .true.

c23456789012345678901234567890123456789012345678901234567890123456789012
c       10        20        30        40        50        60        70



C  include:
      Lgrid = 1
      Lboundary = 9
      Lcontrol = 2
      Lsolver = 5
      Lmaterial = 55


      if(nen .eq. 10) then ! Cubic
      Fgrid ='./Code_Input/GRID10'
      Fboundary = './Code_Input/BOUNDARY10'
      else    ! Quadratic
      Fgrid ='./Code_Input/GRID6'
      Fboundary = './Code_Input/BOUNDARY6'
      endif

      Fcontrol = './Code_Input/CONTROL'
      Fsolver = './Code_Input/SOLVER'
      Fmaterial = './Code_Input/PROPERTIES'

      OPEN(UNIT=Lgrid,FILE=Fgrid,STATUS='OLD')
      OPEN(UNIT=Lboundary,FILE=Fboundary,STATUS='OLD')
      OPEN(UNIT=Lcontrol,FILE=Fcontrol,STATUS='OLD')
C End include:



C================================================================
      print*, '========================='
      print*, 'FEM-Unsteady-Steady-Eigen CFD-Solver Version:2016'
      print*, '========================='


      call read_mesh(Lgrid, TITLE,NDE,NDEmax,x,ndm, ndf,nen,IX,NEL,
     +NELmax,MatGroup,MDF,MEN, nbcV,nbcF,kbc,vbc,lbc,fbc,XX,YY)

      call read_bc(Lboundary, TITLE,NDE,NDEmax,x,ndm, ndf,nen,IX,NEL,
     +NELmax,MatGroup,MDF,MEN, nbcV,nbcF,kbc,VBCo,lbc,fbc)


! Control in following lines mean control of the program not physics !

      call read_control(NMAX,RRHSMAX,REVMax,T0,DT,T,TMAX,NEWMAT,
     +ieig,isteady,NEig,shift, Lcontrol,Fcontrol,
     +NDecompose,PlotTime,kickMe,Nvf)


      call setIA(NDE,NEL,X,IX,NOPP,MDF,KBC, nbcV,IA,JA,ITMP,LDOF,MEN,
     +MatGroup, NDIM,NZMAX,NZLMAX,NZUMAX,NDEmax,NELmax,MatMax, MaxProp,
     +MaxCon,ndf,nen,ndm,F,F1,F2,F3,F4,Neighbour,VolXg,VolYg)


C MM : switched off:    call InitController


      T  =  T0 - DT

      call read_restart_steady(NDE,ndim,iRest,EX,F,F1,F2,F3,F4,
     .                                  NDEmax,NOPP,MDF)

      call read_restart_unsteady(NDE,ndim,iRest,EX,F,F1,F2,
     .    F3,F4,T,isteady,NDEmax,NOPP,MDF,VBC,VBCo,KBC,nbcV,NDF)

      NEWMAT = .TRUE.

C time loop
c##################################################################
7777  CONTINUE
c##################################################################
      T =  T + DT
      NCOUNT=0

C--------Boundary Conditions -I-----for unsteady ------------------

      if(T.ne.0.0.and.isteady.ne.1) 
     &call pressure(NDE,NEL,IX,XX,YY,F,Ndim,PRESS,NOPP,MDF)

      call flow_controller(T,DT,NDE,NEL,IX,NOPP,XX,YY,VolXg, VolYg,
     &                Nvf,
     &                NbcV,KBC,VBCo, VBC,F,PRESS,isteady)


      call set_kick(T,kickMe,NbcV,KBC,VBC,x,ndm,NDEmax,NDF,isteady)

      call update_unsteady(ndim,LDOF,F,F1,F2,F3,F4,dt)
      
      call  set_bcV(NDEmax,ndim,ndf,NOPP,MDF,nbcV,nbcF,kbc,vbc,F)
c      call update_unsteady(ndim,LDOF,F,F1,F2,F3,F4,dt)

C internal loop - Newton-Rapson iteration
c##################################################################
8888  CONTINUE
c##################################################################

      NCOUNT=NCOUNT+1

C zero some vectors etc.------------------------------------
      do i=1,nzmax
         a(i)=0.0
      enddo
      do  k=1,LDOF
         RD(k)=0
      enddo
C zero some vectors etc.-----------------------------------


! Control in following lines mean control of the program not physics !

      call read_control(NMAX,RRHSMAX,REVMax,T0,DT,T,TMAX,NEWMAT,
     +ieig,isteady,NEig,shift, Lcontrol,Fcontrol,
     +NDecompose,PlotTime,kickMe,Nvf)

      call read_material( NMat,MaxProp,MatMax,DMat, Lmaterial,Fmaterial,
     . Re,eps,Bv)


c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C    Eigenvalue calculation

c      if(ieig.eq.1) call eigen(LDOF,A,B,IA,JA,IB,JB,ITMP, NDE,NEL,X,IX,
c     +NOPP,MDF,MEN,T,PlotTime, KBC,VBC,LBC,FBC,nbcV,nbcF,nEig,shift,
c     +REVMax, NDIM,NZMAX,NZLMAX,NZUMAX,NDEmax,NELmax, MatMax,MaxProp,
c     +MaxCon,ndf,nen,ndm,F,F1,F2,F3,F4,RD,Fcontrol,Fsolver,Lcontrol,
c     +Lsolver,xl,ul,NOPPL,MatGroup,Dmat, af,
c     +rdl,dt,JT,IDA,ILMAT,IU,JI,JLMAT,JU,AINV,DR,LMAT,UMAT,DN,DS,XZR,
c     +VECS,R,Q,C,V,D,XD)

C End  Eigenvalue calculation
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      call  set_bcV(NDEmax,ndim,ndf,NOPP,MDF,nbcV,nbcF,kbc,vbc,F)

      call assemble_matrix(NEL,nen,ndf,ndm,MEN,MDF,NOPP, IX,x,xl,
     +ul,NOPPL,MatGroup,Dmat, kbc,VBC,nbcV,IA,JA,A,ITMP,F,F1,F2,
     +F3,F4,
     +RD,VolXg, VolYg,af,rdl,dt, NDIM,NZMAX,NZLMAX,
     +NZUMAX,NDEmax,NELmax,MatMax, MaxProp,MaxCon,LDOF,isteady)
 

C--------Boundary Conditions in RD--------------

      call set_bc_RD(NDEmax,ndim,ndf, NOPP,MDF,nbcV,nbcF,kbc,vbc,RD)

C-End ---Boundary Conditions -I-----for unsteady -------------


C     Load ---Boundary Conditions ---------------

      call load (T,nbcF,lbc,fbc,NOPP,MDF,RD,NDEmax,NDF,NDIM,isteady)

C End Load ---Boundary Conditions ---------------

C   -----  SOLUTION OF THE EQUATION SYSTEM  --------------------------

      OPEN(UNIT=Lsolver,FILE=Fsolver,STATUS='OLD')


      call resid(NDE,NDEmax,ndim,NCOUNT,RD,NOPP,RMAX,T)


      call dsys(A,IA,JA,RD,LDOF,NEWMAT, NDIM,NZMAX,NZLMAX,NZUMAX, JT,
     +IDA,ILMAT,IU,JI,JLMAT,JU,AINV,DR,LMAT,UMAT,DN,DS,XZR, VECS)

      close(UNIT=Lsolver)

      NEWMAT=.TRUE.

      If(mod(nint(T*10000),NDecompose*int(DT*10000)).ne.0.or. NCOUNT
     +.ne.1) NEWMAT = .FALSE.          ! reuse the precond.
      write(*,*)'Ndecompose=',NDecompose,' Decompose=',NEWMAT

C End --  SOLUTION OF THE EQUATION SYSTEM  --------------------------


C------  UPDATING  VALUES --internal step--------------

      call update_internal(LDOF,ndim,F,RD)

C------ Forcing exact values on the boundary ----------------

      call set_bc_RD(NDEmax,ndim,ndf, NOPP,MDF,nbcV,nbcF,kbc,vbc,RD)
      call  set_bcV(NDEmax,ndim,ndf,NOPP,MDF,nbcV,nbcF,kbc,vbc,F)

C End - Forcing exact values on the boundary ----------------

C------ RESIDUUM --RHS-----------------------------------------------

      call resid(NDE,NDEmax,ndim,NCOUNT,RD,NOPP,RMAX,T)

C ----  What to do now ? ----------------------------------

      if(isteady.eq.1.AND.RMAX.LE.RRHSMAX) then

         call pressure(NDE,NEL,IX,XX,YY,F,Ndim,PRESS,NOPP,MDF)

         call V2(NDE,NEL,X,IX,NOPP,MDF,T,PlotTime, NDIM,NZMAX,NZLMAX,
     +   NZUMAX,NDEmax,NELmax,MatMax,MaxProp,MaxCon, ndf,nen,ndm,F,F1,
     +   F2,F3,F4,RD,PRESS)
C FLOW++++++++++++++++++++++++++         stop 'steady solution'

         call write_restart_steady(NDE,ndim,iRest,EX,F,F1,F2,
     +                              F3,F4,Re,NDEmax,NOPP,MDF)
         stop

      endif



      IF(T.LE.TMAX.AND.RMAX.LE.RRHSMAX.AND.isteady.ne.1) THEN

C ---- Convergent time step - do next step -----------


C  ----  Write data for visualisation (or any purpouse)
         call pressure(NDE,NEL,IX,XX,YY,F,Ndim,PRESS,NOPP,MDF)
         call V2(NDE,NEL,X,IX,NOPP,MDF,T,PlotTime, NDIM,NZMAX,NZLMAX,
     +   NZUMAX,NDEmax,NELmax,MatMax,MaxProp,MaxCon, ndf,nen,ndm,F,F1,
     +   F2,F3,F4,RD,PRESS)

        call write_restart_unsteady(NDE,ndim,iRest,EX,F,F1,F2,F3,F4,T
     .             ,NDEmax,NOPP,MDF,VBC,VBCo,KBC,nbcV,NDF)

         GO TO 7777

      ELSE IF ( NCOUNT.GE.NMAX ) THEN

C ----- Hopeless - not convergent after NMAX inner iterations

         call system('touch Covergent_Barrier')

         WRITE(*,*)'***************************************'
         WRITE(*,*)' SOLUTION IS NOT CONVERGENT'
         WRITE(*,*)'***************************************'
         call V2(NDE,NEL,X,IX,NOPP,MDF,T,PlotTime, NDIM,NZMAX,NZLMAX,
     +   NZUMAX,NDEmax,NELmax,MatMax,MaxProp,MaxCon, ndf,nen,ndm,F,F1,
     +   F2,F3,F4,RD,PRESS)


         STOP

      ELSE IF ( T.GT.TMAX ) THEN

C ----- The whole calculation ended succesfully -----------

         call V2(NDE,NEL,X,IX,NOPP,MDF,T,PlotTime, NDIM,NZMAX,NZLMAX,
     +   NZUMAX,NDEmax,NELmax,MatMax,MaxProp,MaxCon, ndf,nen,ndm,F,F1,
     +   F2,F3,F4,RD,PRESS)

         call write_restart_unsteady(NDE,ndim,iRest,EX,F,F1,F2,
     +          F3,F4,T,NDEmax,NOPP,MDF,VBC,VBCo,KBC,nbcV,NDF)

         WRITE(*,*)'***************************************'
         WRITE(*,*)' FINISHED FOR TIME',T
         WRITE(*,*)'***************************************'
         STOP


      ELSE

C ----- Inner loop for a given time step ------------------

C----------- GO TO NEXT inner  LOOP ---------------------------

         GO TO 8888

      endif
      stop

      END

C================================================================
C----------------------------------------------------------------------

      subroutine V2(NDE,NEL,X,IX,NOPP,MDF,T,PlotTime, NDIM,NZMAX,NZLMAX,
     +NZUMAX,NDEmax,NELmax,MatMax,MaxProp,MaxCon, ndf,nen,ndm,F,F1,F2,
     +F3,F4,RD,PRESS)

      Implicit none

      integer ndf,nen,ndm,NDIM,NZMAX,NZLMAX,NZUMAX, NDEmax,NELmax, NDE,
     +NEL,nbcV,nbcF,NMAX, MatMax,MaxProp,MaxCon


      double precision PRESS(NDEmax)

      double precision DT,T0,T,TMAX,RRHSMAX,PlotTime
      double precision F(ndim,5),F1(ndim,5),F2(ndim,5),
     +                 F3(ndim,5),F4(ndim,5),RD(ndim)



      double precision x(ndm,ndemax),dv,da

      integer NOPP(NDEmax),MDF(NDEmax), IX(NEN,NELmax)


      integer i,j,k,l,m,n,NEL4

      CHARACTER*80 FNFLOW, FNPLOT, ARGP
      
      LOGICAL EX


      OPEN(UNIT=9,FILE='Code_Output/Flow.dat',
     .     STATUS='UNKNOWN',FORM='FORMATTED')




      If(mod(nint(T*10000),int(PlotTime*10000)).eq.0 ) then

      if (nen.eq.10) then
      NEL4=NEL*9
      else
      NEL4=NEL*4
      endif



      
      INQUIRE(FILE='Code_Output/Grid.dat',EXIST=EX)
      
      IF(.NOT.EX) then
         write(*,*) 'Writing Grid.dat'
         
       OPEN(UNIT=4,FILE='Code_Output/Grid.dat',
     .     STATUS='UNKNOWN',FORM='FORMATTED')
     
       WRITE(4,*) NDE, 1, 1, 3

      DO  I=1,NDE
          WRITE(4,*) x(1,i),x(2,i)
      ENDDO
      
       WRITE(4,*) NEL4, 3

      DO  I=1,NEL
      if (nen.eq.10) then
  
c-----------------------------------------------------
      WRITE(4,*) abs(IX(1,I)),abs(IX(9,I)),abs(IX(4,I))
      WRITE(4,*) abs(IX(4,I)),abs(IX(9,I)),abs(IX(10,I))
      WRITE(4,*) abs(IX(4,I)),abs(IX(10,I)),abs(IX(5,I))
      WRITE(4,*) abs(IX(5,I)),abs(IX(10,I)),abs(IX(6,I))
      WRITE(4,*) abs(IX(5,I)),abs(IX(6,I)),abs(IX(2,I))
      WRITE(4,*) abs(IX(9,I)),abs(IX(8,I)),abs(IX(10,I))
      WRITE(4,*) abs(IX(10,I)),abs(IX(8,I)),abs(IX(7,I))
      WRITE(4,*) abs(IX(10,I)),abs(IX(7,I)),abs(IX(6,I))
      WRITE(4,*) abs(IX(7,I)),abs(IX(8,I)),abs(IX(3,I))
c-----------------------------------------------------
      else

      WRITE(4,*) abs(IX(1,I)),abs(IX(6,I)),abs(IX(4,I))
      WRITE(4,*) abs(IX(4,I)),abs(IX(6,I)),abs(IX(5,I))
      WRITE(4,*) abs(IX(5,I)),abs(IX(6,I)),abs(IX(3,I))
      WRITE(4,*) abs(IX(4,I)),abs(IX(5,I)),abs(IX(2,I))

      endif

      ENDDO
      
      endif

      ARGP = '000'
      FNFLOW = 'flowfem'//ARGP

      WRITE(ARGP(1:6),'(I6.6)') NINT(T/0.1)

      FNFLOW = 'Code_Output/Flow.'//ARGP

      DO  I=1,NDE  
        WRITE(9,1234) F(NOPP(I),1),F(NOPP(I)+1,1),PRESS(I)
      ENDDO

c      If(mod(nint(T*10000),int(PlotTime*10000)).eq.0 ) then
      write(*,*) 'Plot for time=',T,' started'
      OPEN(UNIT=99,FILE=FNFLOW,STATUS='UNKNOWN',FORM='FORMATTED')
        do  I=1,NDE
          WRITE(99,1234) F(NOPP(I),1),F(NOPP(I)+1,1),PRESS(I)
        enddo
      CLOSE(99)

      write(*,*) 'Plot for time=',T,' ended '

1234   format(1h ,4e30.20)



      CLOSE(4)
      CLOSE(9)

      endif

      RETURN
      END


c###############################################################################




C----------------------------------------------------------------------
C----------------------------------------------------------------------
! OLD Version, writing to flowfem files
      subroutine V2_flowfem
     +(NDE,NEL,X,IX,NOPP,MDF,T,PlotTime, NDIM,NZMAX,NZLMAX,
     +NZUMAX,NDEmax,NELmax,MatMax,MaxProp,MaxCon, ndf,nen,ndm,F,F1,F2,
     +F3,F4,RD,PRESS)

      Implicit none

      integer ndf,nen,ndm,NDIM,NZMAX,NZLMAX,NZUMAX, NDEmax,NELmax, NDE,
     +NEL,nbcV,nbcF,NMAX, MatMax,MaxProp,MaxCon


      double precision PRESS(NDEmax)

      double precision DT,T0,T,TMAX,RRHSMAX,PlotTime
      double precision F(ndim,5),F1(ndim,5),F2(ndim,5),
     +                 F3(ndim,5),F4(ndim,5),RD(ndim)



      double precision x(ndm,ndemax),dv,da

      integer NOPP(NDEmax),MDF(NDEmax), IX(NEN,NELmax)


      integer i,j,k,l,m,n,NEL4

      CHARACTER*80 FNFLOW, FNPLOT, ARGP


      INCLUDE 'plot_dir.inc'

      If(mod(nint(T*10000),int(PlotTime*10000)).eq.0 ) then

      if (nen.eq.10) then
      NEL4=NEL*9
      else
      NEL4=NEL*4
      endif

      WRITE(4,*) NDE,NEL4

      DO  I=1,NDE
          WRITE(4,*) x(1,i),x(2,i)
      ENDDO

      DO  I=1,NEL
      if (nen.eq.10) then
c-----------------------------------------------------
      WRITE(4,*) abs(IX(1,I)),abs(IX(9,I)),abs(IX(4,I))
      WRITE(4,*) abs(IX(4,I)),abs(IX(9,I)),abs(IX(10,I))
      WRITE(4,*) abs(IX(4,I)),abs(IX(10,I)),abs(IX(5,I))
      WRITE(4,*) abs(IX(5,I)),abs(IX(10,I)),abs(IX(6,I))
      WRITE(4,*) abs(IX(5,I)),abs(IX(6,I)),abs(IX(2,I))
      WRITE(4,*) abs(IX(9,I)),abs(IX(8,I)),abs(IX(10,I))
      WRITE(4,*) abs(IX(10,I)),abs(IX(8,I)),abs(IX(7,I))
      WRITE(4,*) abs(IX(10,I)),abs(IX(7,I)),abs(IX(6,I))
      WRITE(4,*) abs(IX(7,I)),abs(IX(8,I)),abs(IX(3,I))
c-----------------------------------------------------
      else

      WRITE(4,*) abs(IX(1,I)),abs(IX(6,I)),abs(IX(4,I))
      WRITE(4,*) abs(IX(4,I)),abs(IX(6,I)),abs(IX(5,I))
      WRITE(4,*) abs(IX(5,I)),abs(IX(6,I)),abs(IX(3,I))
      WRITE(4,*) abs(IX(4,I)),abs(IX(5,I)),abs(IX(2,I))

      endif

      ENDDO

      ARGP = '000'
      FNFLOW = 'flowfem'//ARGP

      WRITE(ARGP(1:10),'(e10.5)') T

      FNFLOW = 'Code_Output/flowfem'//ARGP

      DO  I=1,NDE  
!         WRITE(9,1234) PRESS(I), sin(x(1,i)),sin(x(2,i))
        WRITE(9,1234) PRESS(I),F(NOPP(I),1),F(NOPP(I)+1,1)
      ENDDO

c      If(mod(nint(T*10000),int(PlotTime*10000)).eq.0 ) then
      write(*,*) 'Plot for time=',T,' started'
      OPEN(UNIT=99,FILE=FNFLOW,STATUS='UNKNOWN',FORM='FORMATTED')
        do  I=1,NDE
          WRITE(99,1234) PRESS(I),F(NOPP(I),1),F(NOPP(I)+1,1)
        enddo
      CLOSE(99)

      write(*,*) 'Plot for time=',T,' ended '

1234   format(1h ,4e12.4)



      CLOSE(4)
      CLOSE(9)

      endif

      RETURN
      END


c###############################################################################









