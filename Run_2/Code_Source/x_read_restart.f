      subroutine read_restart_unsteady(NDE,ndim,iRest,EX,F,F1,F2,
     .        F3,F4,T,isteady,NDEmax,NOPP,MDF,VBC,VBCo,KBC,NBC,NDF)
 
      implicit none
 
      integer i,j,iRest,ndim,NDE,isteady,NDEmax,NDF
      real*8 F(ndim,5),F1(ndim,5),F2(ndim,5),
     .       F3(ndim,5),F4(ndim,5),T

      integer NOPP(NDEmax),MDF(NDEmax)

      real*8 VBC(NDEmax,NDF),VBCo(NDEmax,NDF)
      integer KBC(NDEmax,NDF+1),NBC

      character*200 InputLine


      LOGICAL EX
 
C ----- RESTART FILES -----------------------------------

      if (isteady.eq.1) return
      
      iRest=84
      INQUIRE(FILE='Code_Input/Restart_unsteady',EXIST=EX)
      IF(.NOT.EX) then
         write(*,*) 'No restart file Restart_unsteady'
      else
         OPEN(UNIT=iRest,FILE='Code_Input/Restart_unsteady',
     .   STATUS='OLD',FORM='FORMATTED')
       do i=1,NDE 
         Read (iRest, '(A200)' ) InputLine
         Read(Unit=InputLine, FMT=* )NOPP(i),MDF(i)
c         write(*,*)NOPP(i),MDF(i)
         if(MDF(i).eq.3) then
         Read(Unit=InputLine, FMT=* ) 
     .   NOPP(i),MDF(i),F(NOPP(I),1),F(NOPP(I)+1,1),F(NOPP(I)+2,1)
c         write(*,*) 
c     .   NOPP(i),MDF(i),F(NOPP(I),1),F(NOPP(I)+1,1),F(NOPP(I)+2,1)
         endif
         if(MDF(i).eq.2)
     .   Read(Unit=InputLine, FMT=* )
     .   NOPP(i),MDF(i),F(NOPP(I),1),F(NOPP(I)+1,1)
       enddo 

        do i=1,NDE 
         if(MDF(i).eq.3) then
         read(iRest,*)
     .   F1(NOPP(I),1),F1(NOPP(I)+1,1),F1(NOPP(I)+2,1)
         endif
         if(MDF(i).eq.2)
     .   read(iRest,*)
     .   F1(NOPP(I),1),F1(NOPP(I)+1,1) 
       enddo 

    
        do i=1,NDE 
         if(MDF(i).eq.3) then
         read(iRest,*)
     .   F2(NOPP(I),1),F2(NOPP(I)+1,1),F2(NOPP(I)+2,1)
         endif
         if(MDF(i).eq.2)
     .   read(iRest,*)
     .   F2(NOPP(I),1),F2(NOPP(I)+1,1) 
       enddo 

     
        do i=1,NDE 
         if(MDF(i).eq.3) then
         read(iRest,*)
     .   F3(NOPP(I),1),F3(NOPP(I)+1,1),F3(NOPP(I)+2,1)
         endif
         if(MDF(i).eq.2)
     .   read(iRest,*)
     .   F3(NOPP(I),1),F3(NOPP(I)+1,1) 
       enddo 

     
        do i=1,NDE 
         if(MDF(i).eq.3) then
         read(iRest,*)
     .   F4(NOPP(I),1),F4(NOPP(I)+1,1),F4(NOPP(I)+2,1)
         endif
         if(MDF(i).eq.2)
     .   read(iRest,*)
     .   F4(NOPP(I),1),F4(NOPP(I)+1,1) 
       enddo 

	 read(iRest,*) T

      do  I=2,NDF+1
            do  J=1,NBC
               if (KBC(J,I).NE.0) then
                  VBC(J,I-1) =  VBCo(J,I-1)
               end if
            end do
         end do

	 
         write(*,*) 'Restart values read from Restart_unsteady'
cccc          T = T - DT
      endif
C END ----- RESTART FILES ---------------------------------
 
      return
      end
 
      subroutine read_restart_steady(NDE,ndim,iRest,EX,F,F1,F2,F3,F4,
     .                                  NDEmax,NOPP,MDF)
 
      implicit none

      integer i,j,iRest,ndim,NDE,NDEmax
      real*8 F(ndim,5),F1(ndim,5),F2(ndim,5),
     .       F3(ndim,5),F4(ndim,5),T

      integer NOPP(NDEmax),MDF(NDEmax)

      character*200 InputLine

      
      LOGICAL EX
 
C ----- RESTART FILES -----------------------------------
      iRest=83
      INQUIRE(FILE='Code_Input/Restart_steady',EXIST=EX)
      IF(.NOT.EX) then
         write(*,*) 'No restart file Restart_steady'
      else
         OPEN(UNIT=iRest,FILE='Code_Input/Restart_steady',
     .   STATUS='OLD',FORM='FORMATTED')
       do i=1,NDE 
         Read (iRest, '(A200)' ) InputLine
         Read(Unit=InputLine, FMT=* )NOPP(i),MDF(i)
c         write(*,*)NOPP(i),MDF(i)
         if(MDF(i).eq.3) then
         Read(Unit=InputLine, FMT=* ) 
     .   NOPP(i),MDF(i),F(NOPP(I),1),F(NOPP(I)+1,1),F(NOPP(I)+2,1)
c         write(*,*) 
c     .   NOPP(i),MDF(i),F(NOPP(I),1),F(NOPP(I)+1,1),F(NOPP(I)+2,1)
         endif
         if(MDF(i).eq.2)
     .   Read(Unit=InputLine, FMT=* )
     .   NOPP(i),MDF(i),F(NOPP(I),1),F(NOPP(I)+1,1)
       enddo 
       
       do i=1,ndim 
         F1(i,1) = F(i,1)
         F2(i,1) = F(i,1)
         F3(i,1) = F(i,1)
         F4(i,1) = F(i,1)
       enddo 
       	 
         write(*,*) 'Restart values read from Restart_steady'
      endif
C END ----- RESTART FILES ---------------------------------
 
      return
      end
          
      
      subroutine write_restart_unsteady(NDE,ndim,iRest,EX,F,F1,F2,
     .                                   F3,F4,T,NDEmax,NOPP,MDF)
 
      implicit none
 
      integer i,iRest,ndim,NDE,NDEmax
      real*8 F(ndim,5),F1(ndim,5),F2(ndim,5),
     .       F3(ndim,5),F4(ndim,5),T

      integer NOPP(NDEmax),MDF(NDEmax)


      LOGICAL EX
 
C ----- RESTART FILES -----------------------------------
      iRest=60
         OPEN(UNIT=iRest,FILE='Code_Input/Restart_unsteady_T',
     .   FORM='FORMATTED')
       do i=1,NDE 
         if(MDF(i).eq.3)
     .   write(iRest,*) 
     .   NOPP(i),MDF(i),F(NOPP(I),1),F(NOPP(I)+1,1),F(NOPP(I)+2,1)
         if(MDF(i).eq.2)
     .   write(iRest,*) NOPP(i),MDF(i),F(NOPP(I),1),F(NOPP(I)+1,1)
       enddo 
        do i=1,NDE 
         if(MDF(i).eq.3)
     .   write(iRest,*) 
     .   F1(NOPP(I),1),F1(NOPP(I)+1,1),F1(NOPP(I)+2,1)
         if(MDF(i).eq.2)
     .   write(iRest,*) F1(NOPP(I),1),F1(NOPP(I)+1,1)
       enddo 
       do i=1,NDE 
          if(MDF(i).eq.3)
     .   write(iRest,*) 
     .   F2(NOPP(I),1),F2(NOPP(I)+1,1),F2(NOPP(I)+2,1)
         if(MDF(i).eq.2)
     .   write(iRest,*) F2(NOPP(I),1),F2(NOPP(I)+1,1)
       enddo 
        do i=1,NDE 
         if(MDF(i).eq.3)
     .   write(iRest,*) 
     .   F3(NOPP(I),1),F3(NOPP(I)+1,1),F3(NOPP(I)+2,1)
         if(MDF(i).eq.2)
     .   write(iRest,*) F3(NOPP(I),1),F3(NOPP(I)+1,1)
       enddo 
       do i=1,NDE 
           if(MDF(i).eq.3)
     .   write(iRest,*) 
     .   F4(NOPP(I),1),F4(NOPP(I)+1,1),F4(NOPP(I)+2,1)
         if(MDF(i).eq.2)
     .   write(iRest,*) F4(NOPP(I),1),F4(NOPP(I)+1,1)
       enddo 
	 write(iRest,*) T
	 close(iRest)
	 
C END -----WRITE  RESTART FILE ---------------------------------
 
      return
      end
 
 
       
      subroutine write_restart_steady(NDE,ndim,iRest,EX,F,F1,F2,
     .                                 F3,F4,Re,NDEmax,NOPP,MDF)
 
      implicit none
 
      integer i,iRest,ndim,NDE,NDEmax
      real*8 F(ndim,5),F1(ndim,5),F2(ndim,5),
     .       F3(ndim,5),F4(ndim,5),T,Re
      integer NOPP(NDEmax),MDF(NDEmax)


      CHARACTER*80 FNFLOW, FNPLOT, ARGP
 
      LOGICAL EX
 
C ----- RESTART FILES -----------------------------------

       ARGP = '0000000'
       

      WRITE(ARGP,'(I4.4)') int(Re)
      
      iRest=60
         OPEN(UNIT=iRest,FILE='Code_Input/Restart_steady_'//ARGP,
     .   FORM='FORMATTED')
       do i=1,NDE 
         if(MDF(i).eq.3)
     .   write(iRest,*) 
     .   NOPP(i),MDF(i),F(NOPP(I),1),F(NOPP(I)+1,1),F(NOPP(I)+2,1)
         if(MDF(i).eq.2)
     .   write(iRest,*) NOPP(i),MDF(i),F(NOPP(I),1),F(NOPP(I)+1,1)
       enddo 
       
	 close(iRest)
      
	 
C END -----WRITE  RESTART FILE ---------------------------------
 
      return
      end
 
 

 
 
 
 
 
 
