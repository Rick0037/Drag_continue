       	program MF32TEC

        implicit none

        REAL, DIMENSION(:)  , ALLOCATABLE :: X,Y,Vx,Vy,P,VORT
        REAL :: U1,U2,U3,U4,V1,V2,V3,V4,X1,X2,X3,X4,Y1,Y2,Y3,Y4
        REAL :: DX,DY,AREA,CIRC,Vort_max,Vort_min
        INTEGER, DIMENSION(:,:)  , ALLOCATABLE :: IX
        INTEGER, DIMENSION(:,:)  , ALLOCATABLE :: ICON
        INTEGER, DIMENSION(:)  , ALLOCATABLE :: IND
        INTEGER :: NDE,NEL,NEL4,i,j,k,K1,K2,K3,K4
       



!———————————————
       open(unit=4,file='../Code_Input/GRID6',err=1000,status='OLD')
!———————————————
       open(unit=9,file='../Code_Output/Flow.dat',err=1000,status='OLD')

       open(76,file='UNS3.vtk',form='FORMATTED',status='UNKNOWN')


       read(4,*)
       read(4,*) NDE

       do i=1,NDE
        read(4,*)
       enddo
       read(4,*)
       read(4,*) NEL
       Rewind(4)

       ALLOCATE (X(NDE),Y(NDE),Vx(NDE),Vy(NDE),P(NDE),VORT(NDE),IND(NDE))
       ALLOCATE (IX(6,NDE))
       ALLOCATE (ICON(4*NEL,3))
       


       write(76,'(A)')'# vtk DataFile Version 1.0'
       write(76,'(A)')'UNS3.vtk  3D Unstructured Grid of Triangles'
       write(76,'(A)')'ASCII'
       write(76,*)
       write(76,'(A)')'DATASET UNSTRUCTURED_GRID'
       write(76,'(A6,I8,A6)')'POINTS',NDE,' float'
       read(4,*)
       read(4,*) NDE

       do i=1,NDE
        read(4,*) X(i),Y(i)
        read(9,*) Vx(i), Vy(i), P(i)
       enddo

       read(4,*)
       read(4,*) NEL

       do i=1,NEL
        read(4,*) (IX(j,i),j=1,6)
       enddo


!————————————————————
!—— Calculate Vorticity
!————————————————————



      do i=1,NEL
       ICON(4*I-3,1:3) = (/abs(IX(1,I)) , abs(IX(6,I)) , abs(IX(4,I)) /)
       ICON(4*I-2,1:3) = (/abs(IX(4,I)) , abs(IX(6,I)) , abs(IX(5,I)) /)
       ICON(4*I-1,1:3) = (/abs(IX(5,I)) , abs(IX(6,I)) , abs(IX(3,I)) /)
       ICON(4*I-0,1:3) = (/abs(IX(4,I)) , abs(IX(5,I)) , abs(IX(2,I)) /)
      enddo


       DO i = 1, NDE
         VORT(i)  = 0.0
       ENDDO

       Vort_max=0.0
       Vort_min=0.0

       DO i = 1, NDE
         IND(i)  = 0
       ENDDO



       DX = 0.0
       DY = 0.0
       DO i = 1, NEL*4

         K1= ICON(i,1)
         K2= ICON(i,2)
         K3= ICON(i,3)
         K4= ICON(i,1)

         X1 = X(K1)
         X2 = X(K2)
         X3 = X(K3)
         X4 = X(K4)
         DX = MAX(DX, MAX(X1,X2,X3,X4)-MIN(X1,X2,X3,X4))
         Y1 = Y(K1)
         Y2 = Y(K2)
         Y3 = Y(K3)
         Y4 = Y(K4)
         DY = MAX(DY, MAX(Y1,Y2,Y3,Y4)-MIN(Y1,Y2,Y3,Y4))
         U1 = Vx(K1)
         V1 = Vy(K1)
         U2 = Vx(K2)
         V2 = Vy(K2)
         U3 = Vx(K3)
         V3 = Vy(K3)
         U4 = Vx(K4)
         V4 = Vy(K4)

         AREA = ( (X3-X1)*(Y4-Y2) + (X2-X4)*(Y3-Y1) )
         CIRC = ( (U1+U2)*(X2-X1) + (V1+V2)*(Y2-Y1)   &
                + (U2+U3)*(X3-X2) + (V2+V3)*(Y3-Y2)   &
                + (U3+U4)*(X4-X3) + (V3+V4)*(Y4-Y3)   &
                + (U4+U1)*(X1-X4) + (V4+V1)*(Y1-Y4) ) / AREA

         VORT(K1) = VORT(K1) + CIRC
         IND(K1)  = IND(K1)  + 1
         VORT(K2) = VORT(K2) + CIRC
         IND(K2)  = IND(K2)  + 1
         VORT(K3) = VORT(K3) + CIRC
         IND(K3)  = IND(K3)  + 1

         IF(K4.NE.K1) then
            VORT(K4) = VORT(K4) + CIRC
            IND(K4)  = IND(K4)  + 1
         ENDIF

      ENDDO

       do i=1,NDE
        write(76,'(3E13.5)') X(i),Y(i),0.0
       enddo


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        DO J=1,NDE
          VORT(J) = VORT(J) / FLOAT (IND(J))
          if (Vort_max.lt.VORT(J)) Vort_max=VORT(J)
          if (Vort_min.gt.VORT(J)) Vort_min=VORT(J)
        ENDDO

        write(*,*) 'VORT Max  Min  ', Vort_max,Vort_min

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      



       NEL4 =4*NEL

!  Copy the connectivity information.
!  VTK will want 0-based indices, so actually read the data as integers.
!
    write(76,'(A,2I10)')  'CELLS', NEL4, ( 3 + 1 ) * NEL4


       do i=1,NEL
      WRITE(76,*) 3, abs(IX(1,I)) - 1,abs(IX(6,I)) - 1,abs(IX(4,I)) - 1
      WRITE(76,*) 3, abs(IX(4,I)) - 1,abs(IX(6,I)) - 1,abs(IX(5,I)) - 1
      WRITE(76,*) 3, abs(IX(5,I)) - 1,abs(IX(6,I)) - 1,abs(IX(3,I)) - 1
      WRITE(76,*) 3, abs(IX(4,I)) - 1,abs(IX(5,I)) - 1,abs(IX(2,I)) - 1
       enddo



       WRITE(76,'(A,I10)') 'CELL_TYPES ', NEL4

       do i=1,NEL4
          WRITE(76,*) 5
       enddo




      WRITE(76,'(A,I8)')'POINT_DATA ', NDE 
      WRITE(76,'(3A)') 'VECTORS',' velocity',' float'
      DO  I=1,NDE
          WRITE(76,'(3E13.5)') Vx(I),Vy(I),0.0
      ENDDO

      WRITE(76,'(3A,I2)') 'SCALARS',' pressure',' float',1
      WRITE(76,'(A)') 'LOOKUP_TABLE default'
      WRITE(76,'(1E13.5)') (P(I),I=1,NDE)

      WRITE(76,'(3A,I2)') 'SCALARS',' vorticity',' float',1
      WRITE(76,'(A)') 'LOOKUP_TABLE default'
      WRITE(76,'(1E13.5)') (VORT(I),I=1,NDE)



!       write(76,*) 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX'

       stop

 1000  write(*,*)'Error in read'

        stop
        end



