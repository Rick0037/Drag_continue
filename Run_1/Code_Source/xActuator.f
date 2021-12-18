************************************************************************
* === subroutine which determines the cylinder motion and ============ *
* === adjusts the volume force and boundary condition correspondingly  *
************************************************************************
* --- created by Marek Morzynski, 20016-05-17 ------------------------- *
************************************************************************

      subroutine flow_controller(T, DT, NDE,NEL, IX, NOPP,
     &                      XX, YY, VolXg, VolYg, Nvf,
     &                      NBC,KBC,VBCo, VBC,F,PRESS,isteady)
*     T     = time
*     DT    = time step /increment per step
*     NDE   = number of points in grid
*     NEL   = number of elements
*     IX    = Connectivity matrix for FEM grid
*     NOPP  = Pointer to the first DOF at the element
*     XX    = x-coordinate of grid points
*     YY    = y-coordinate of grid points
*     VolXg = volume force in x direction in nodes ( NDE vector)
*     VolYg = volume force in y direction in nodes ( NDE vector)
*     Nvf   = Number of volume forces set in Code_Input in file CONTROL (mostly 0 - historical use)
*     NBC   = number of BC-nodes
*     KBC   = ?
*     VBCo  = values of BC in the "laboratory" frame, as set in BOUNDARY6 in Code_Input directory
*     VBC   = actual values of BC in "body" frame as set by the program (controller)


      implicit none


      integer ndf,nen,ndm,NZMAX,NZLMAX,NZUMAX, NDEmax,NDIM,NELmax, NDE,
     +NEL,nbcV,nbcF,NMAX,MatMax,MaxProp,MaxCon,MaxEig,NvfP,Nvf
C     Include parameters - here the size of the problem is defined
      include 'ndf.inc'
      include 'parameter.inc'

C     Values of computed vectors for t
      double precision F(ndim,5)

C     NOPP points to first DOF of a node (NOPPL - the same locally),
      integer NOPP(NDEmax)

C     Connectivity matrix
      integer IX(NEN,NELmax)

C     Boundary conditions v
      double precision VBC(NDEmax,NDF)
      integer KBC(NDEmax,NDF+1)

      integer isteady
      logical ex

      double precision DT,T,PI

      double precision XX(NDEmax),YY(NDEmax),
     .          VBCo(NDEmax,NDF)
      double precision      VolXg(NDEmax), VolYg(NDEmax)

      double precision U1(NDEmax), V1(NDEmax), PRESS(NDEmax)
      integer Lvolume, NGrid, NBC  !!!, Nvf
      character*80   Fvolume
      double precision Vol_X(NvfP,NDEmax),Vol_Y(NvfP,NDEmax)
      double precision VelocityX, VelocityY
      double precision AccelerationX(NvfP),AccelerationY(NvfP)
*   + counter
      integer i, j, n, K, iVF, Nnod, iGrid

****** BEGIN OF MAIN SUBROUTINE SEGMENT *******************************
** == Initialization
      NGrid  = NDE
* === Initialize volume force and boundary condition ===================
* = Rather historical way of introducing volume forces, now use external routine
* = Rather historical way of introducing volume forces, now use external routine
      if (T.le.0.1) then   !   oscillation  AFTER t = .....
         Lvolume=44
         do i=1,Nvf
            do j=1,NGrid
               Vol_X(i,j)=0.0
               Vol_Y(i,j)=0.0
            end do
         end do

         if (isteady.eq.0) then ! only for unsteady computation
            do i=1,Nvf
               Fvolume ='./Code_Input/VOLUME_FORCE_'
               write(Fvolume,'(a,i3.3)') './Code_Input/VOLUME_FORCE_',i
               write(*,*) Fvolume, i
               OPEN(UNIT=Lvolume,FILE=Fvolume,
     &              STATUS='OLD',form='formatted')
               Read(Lvolume,*) iVf, Nnod
               do j=1,Nnod
                  Read(Lvolume,*)  n, Vol_X(iVf,n), Vol_Y(iVf,n)
               end do
               close(Lvolume)
            end do
         end if ! only for unsteady computation

*     ++ Zero volume force and BC not changed
         do  I=2,NDF+1
            do  J=1,NBC
               if (KBC(J,I).NE.0) then
                  VBC(J,I-1) =  VBCo(J,I-1)
               end if
            end do
         end do

         do iGrid=1,NGrid
            VolXg(iGrid) = 0.0D0
            VolYg(iGrid) = 0.0D0
         end do
* = Rather historical way of introducing volume forces, now use external routine
* = Rather historical way of introducing volume forces, now use external routine
************************************************************************
*     ++ steps T=0.2 and later
      else
*         if t> 0.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


          call system('rm -rf Control_Barrier Control_Input.dat')

          OPEN(UNIT=60,FILE="Control_Input.dat",FORM="UNFORMATTED")

*  Write all field, grid, time data for controller

          write(60)T, DT, NDE,NEL,NDEmax,NELmax, IX, NOPP,
     &                      XX, YY, VolXg, VolYg, Nvf,
     &                      NBC,KBC,VBCo, VBC,F,PRESS,isteady

          close(60)
          close(65)

* Set Control_Barrier 

          OPEN(UNIT=65,FILE="Control_Barrier",STATUS='NEW')

          ex= .true.

* Waites till Control_Barrier is removed by external action (matlab/octave)

          Do While (ex)
             INQUIRE (FILE="Control_Barrier", EXIST=ex)
          End Do

* Reads controller actuation (Dirichlet BC and Volume Forces) to the flow solver

          OPEN(UNIT=60,FILE="Control_Output.dat",FORM="UNFORMATTED",
     +    access="stream")

          Read(60) VBC, VolXg, VolYg

          close(60)

         endif

         return
         end




