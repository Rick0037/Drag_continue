      subroutine setIA(NDE,NEL,X,IX,NOPP,MDF,KBC,
     .         nbcV,IA,JA,ITMP,LDOF,MEN,MatGroup,
     .        NDIM,NZMAX,NZLMAX,NZUMAX,NDEmax,NELmax,MatMax,
     .        MaxProp,MaxCon,ndf,nen,ndm,F,F1,F2,
     .        F3,F4,Neighbour,VolXg,VolYg)

      Implicit none

      integer    NDIM,NZMAX,NZLMAX,NZUMAX, 
     .           NDEmax,NELmax,    
     .           NDE,NEL,nbcV,nbcF,NMAX,  
     .           MatMax,MaxProp,MaxCon,
     .           ndf,nen,ndm


      integer II,Mat,isw
      integer NOPP(NDEmax),MDF(NDEmax),MEN(NELmax),
     .          IX(nen,NELmax)

      integer MatGroup(NELmax,2) 
 
      INTEGER IA(NDIM+1), JA(NZMAX),ITMP(NDIM+1)
      integer KBC(NDEmax,NDF+1),LDOF
 
      LOGICAL BOUNDARY
      integer Neighbour(NDEmax,MaxCon),NODE,NODE1,NODE2,NODEt 
      integer i,j,k,l,m,n,jj,kk,jjj,kkk,LengthRow,Icol
      real*8 Dist,DistMin

C     Nodal coordinates
      real*8 x(ndm,ndemax) 


C-------These matrices are only once set to zero -----------
 
      real*8 F(ndim,5),F1(ndim,5),F2(ndim,5),F3(ndim,5),F4(ndim,5)

   
      double precision      VolXg(NDEmax), VolYg(NDEmax)


  
      do  i=1,ndim
           do j=1,5
               f1(i,j)=0.0
               f2(i,j)=0.0
               f3(i,j)=0.0
               f4(i,j)=0.0
               f (i,j)=0.0
            enddo
       enddo
    
C-------These matrices are only once set to zero -----------
 
   
       do i=1,NDEmax
          VolXg(i)=0.0D0
          VolYg(i)=0.0D0
         do j=1,MaxCon
           Neighbour(i,j)=0
         enddo
       enddo 

C-----Check if the point is on the boundary ---------
      do   i=1,NEL
          do  j=1,MEN(i)
               CALL CHECKBOUNDARY (IX(j,i),KBC,nbcV,NDEmax,NDF)  
          enddo
      enddo

C-----Check if the point is on the boundary ---------

C----- Determine numbers of DOF ---------------------
      NOPP(1) = 1
      do  I=2,NDE
           NOPP(I) = NOPP(I-1) + MDF(I-1)   ! I DOF of node 
      enddo
        LDOF = NOPP(NDE) + MDF(NDE) -1 
        write(*,*) 'last degree of freedom ', LDOF
C-End of - Determine numbers of DOF ---------------------

C Determine neighbouring nodes for each one

 
         
        do kk=1,NDE
           NODE1=kk
                         
             do j=1,NEL
               do i=1, MEN(j)
                    NODEt=abs(IX(i,j))
                    if(NODE1.eq.NODEt) then
                 
                                 do n=1,MEN(j) 
                         NODE2=ABS(IX(n,j))  
                         ii=0
30                       ii=ii+1      ! analizowany 
                    if(Neighbour(kk,ii).eq.0) then
                        if(ii.eq.1) then
                           Neighbour(kk,ii)=abs(NODE2)                     
                           go to 40
                        else if (abs(NODE2).gt.Neighbour(kk,ii-1))then
                          Neighbour(kk,ii)=abs(NODE2)
                          go to 40
                        else
                          ii=0
                          go to 30
                        endif
                   else if (Neighbour(kk,ii).eq.ABS(NODE2))then
                        go to 40
                   else if (Neighbour(kk,ii).gt.ABS(NODE2)) then
                        do m=MaxCon-1,ii,-1
                    Neighbour(kk,m+1) = Neighbour(kk,m)
                        end do
                        Neighbour(kk,ii)=ABS(NODE2)
                        go to 40
                   else if (Neighbour(kk,ii).lt.ABS(NODE2)) then
                        go to 30
                   else
                         write(*,*) 'error',ii,Neighbour(kk,ii),node2
                         stop 'error in data'
                   endif                          
40      continue

                              enddo

                    endif !   if(NODE1.eq.NODEt)
                enddo
            enddo
c            write(*,*) kk,(Neighbour(kk,ii),ii=1,30)
          enddo
  
  

  
C End Determine neighbouring nodes for each one       

         
          do i=1, NDE
           if(Neighbour(i,1).eq.0) 
c     .     write(*,*)i,(Neighbour(i,ii),ii=1,30)
     .    write(*,*)'Warning: Node ',i,' not connected to any element'

          enddo

	do i=1,NEL
         DistMin=1.0D30
         do j=1,MEN(i)
              NODE1=abs(IX(j,i))
            if(j.eq.MEN(i))then 
              NODE2=abs(IX(1,i))
            else
              NODE2=abs(IX(j+1,i))
            endif
            
            DistMin=1.0D30
            Dist=0.0D0
            do k=1,ndm
                   Dist=Dist+
     .              ( x(k,NODE1)-x(k,NODE2) ) **2
            enddo 
                   Dist=sqrt(Dist)  
                   DistMin=Min(DistMin,Dist) 
         enddo
c       if (DistMin.lt.1.0D-3.and. MatGroup(i,1).ne.100) then ! not for mass element (100)
         if (DistMin.lt.1.0D-5) then ! not for mass element (100)
        write (*,*) 'Warning: (Almost) zero element', i, '  Nodes ',
     . (abs(IX(k,i)),k=1,MEN(i))
       write (*,*)'Element ', i,' Minimum nodes distance is ',DistMin
      
        write (*,'(i6,3e12.4)')
     .  (abs(IX(n,i)) , (x(m, abs(IX(n,i))), m=1,ndm),n=1,MEN(i))
c           stop
             endif 
      enddo


      do i=1,LDOF+1
         ia(i)  = 1
         itmp(i)= 0
      enddo


      do i=1,nzmax
         ja(i)=nzmax+1
      enddo

c       go to 1111
       Icol=0
       do i=1,NDE        !  Loop over nodes
        do j=1,MDF(i)
         LengthRow=0   
         
         do k=1,MaxCon 
            if(Neighbour(i,k).ne.0) then
              LengthRow= LengthRow + MDF(Neighbour(i,k))
                 do m=1,MDF(Neighbour(i,k))
                     Icol=Icol+1
                      JA(Icol)=NOPP(Neighbour(i,k)) + m - 1
                 enddo
            endif
         enddo
       if(MDF(i).ne.0) 
     .  ia(nopp(i) + j ) =  ia(nopp(i) + j - 1)
     .                               +  LengthRow 
        enddo
      enddo 

       write(*,*) 'IA matrix set'
      
1111   continue

       return
       end
C **********************************************************************
      subroutine skset(i,j,val, A,IA,JA,ITMP,LDOF,
     .        NDIM,NZMAX)
C **********************************************************************
      Implicit none

      integer LDOF,NDIM,NZMAX
      INTEGER IERR, INF, NCOL, NROW
      INTEGER IA(NDIM+1), JA(NZMAX),ITMP(NDIM+1)
      real*8 A(NZMAX),val

      integer p,istart,iend,ii,i,j
      istart=ia(j)
      iend=ia(j+1)-1
      ii=istart-1
 30   ii=ii+1
      if(ii.gt.nzmax) stop 'dimension too small'
      if(.not.(ii.gt.iend.or.ja(ii).ge.i)) goto 30
      if(ii.gt.iend.or.ja(ii).gt.i) then
        do 10 p=ia(ldof+1),ii+1,-1
          a(p)=a(p-1)
          ja(p)=ja(p-1)
 10     continue
        do 20 p=j+1,ldof+1
          ia(p)=ia(p)+1
 20     continue
        ja(ii)=i
        a(ii)=val
      elseif(ja(ii).eq.i) then
        a(ii)=val
      else
        stop 'something wrong with data structure'
      endif
      end
C **********************************************************************
      subroutine skadd(i,j,val, A,IA,JA,ITMP,LDOF,
     .        NDIM,NZMAX)
C **********************************************************************
      Implicit none

      integer LDOF,NDIM,NZMAX
      INTEGER IERR, INF, NCOL, NROW
      INTEGER IA(NDIM+1), JA(NZMAX),ITMP(NDIM+1)
      real*8 A(NZMAX),val

      integer p,istart,iend,ii,i,j
      istart=ia(j)
      iend=ia(j+1)-1
      ii=istart-1
 30   ii=ii+1
      if(ii.gt.nzmax) stop 'dimension too small'
      if(.not.(ii.gt.iend.or.ja(ii).ge.i)) goto 30
      if(ii.gt.iend.or.ja(ii).gt.i) then
        do 10 p=ia(ldof+1),ii+1,-1
          a(p)=a(p-1)
          ja(p)=ja(p-1)
 10     continue
        do 20 p=j+1,ldof+1
          ia(p)=ia(p)+1
 20     continue
        ja(ii)=i
        a(ii)=val
      elseif(ja(ii).eq.i) then
        a(ii)=val+a(ii)
      else
        stop 'something wrong with data structure'
      endif
      end


 

C **********************************************************************
      subroutine checkboundary(IXJI,KBC,NBC,NDEmax,NDF)
C **********************************************************************

 

      DIMENSION KBC(NDEmax,NDF+1)
    
      DO 1 N=1,NBC

      IF (  KBC(N,1) .EQ. ABS ( IXJI ) ) THEN

             IXJI = - ABS ( IXJI )

      END IF
1     CONTINUE
      
      RETURN
      END

C **********************************************************************
      subroutine CHECKDOF(L,IXJI,KBC,NBC,BOUNDARY,NDEmax,NDF)
C **********************************************************************

 
      LOGICAL   BOUNDARY
      DIMENSION KBC(NDEmax,NDF+1)

  
      
      BOUNDARY=.false.

      DO 1 N=1,NBC

      IF ( KBC(N,1) .EQ. -IXJI .AND. KBC(N,L+1) .NE. 0 ) THEN

             BOUNDARY= .TRUE.

             RETURN
      END IF

1     CONTINUE
      
      RETURN
      END















