      subroutine read_bc(Lboundary, TITLE,NDE,NDEmax,x,ndm, ndf,nen,IX,
     +NEL,NELmax,MatGroup,MDF,MEN, nbcV,nbcF,kbc,VBCo,lbc,fbc)
 
 
      implicit none
 
      character*80 InputLine, TITLE
 
      integer ndf,nen,ndm, NDEmax,NELmax, NDE,NEL,nbcV,nbcF,NMAX,
     +MatMax,MaxProp,MaxCon,MaxEig
 
       integer i,j,k,l,m,n,iElem, iMat,Lgrid,Lboundary,Lcontrol,Lsolver, 
     +Lmaterial,LDOF,NMat,iRest,isw,NCOUNT, KK,JJ,KG,JG,KG0,JG0,
     +KL,JL,isteady,ieig,ipass,NDecompose, NEig

      integer nbc, nbcp, node
      real*8 pval
  
 
 
C     Nodal coordinates
      real*8 x(ndm,ndemax)
 
 
      integer MDF(NDEmax),MEN(NELmax)
 
C     Connectivity matrix
      integer IX(NEN,NELmax)
 
C     Boundary conditions v
      real*8 VBCo(NDEmax,NDF)
      integer KBC(NDEmax,NDF+1)
 
C     Boundary conditions F
      real*8 FBC(NDEmax,NDF)
      integer LBC(NDEmax,NDF+1)
 
C     Material Properties
      integer MatGroup(NELmax,2)
 
      
      READ(Lboundary,*)
      READ(Lboundary,*) nbcV
      WRITE(*,*) NbcV,' Boundary conditions '
      do i=1,nbcV
         read(Lboundary,*) (kbc(i,k),k=1,3),(VBCo(i,k),k=1,2)
      enddo

      READ(Lboundary,*)
      READ(Lboundary,*) nbcp
      nbc=nbcv
      do 14 i=1,nbcp
        READ(Lboundary,*) node,pval
        do 144 j=1,nbcv
          if (node.eq.kbc(j,1)) then
            kbc(j,4)=1
            VBCo(j,3)=pval
            go to 14
          end if
144    continue
        nbc=nbc+1
        kbc(nbc,1)=node
        kbc(nbc,2)=0
        kbc(nbc,3)=0
        kbc(nbc,4)=1
        VBCo(nbc,1)=0
        VBCo(nbc,2)=0
        VBCo(nbc,3)=pval
 14   continue

      nbcV=nbc



      return
      
      
C    Exit here - no RHS (stress) BC needed in flow       
      
      
      READ(Lboundary,*)
      READ(Lboundary,*) nbcF
      do i=1,nbcF
         read(Lboundary,*) (lbc(i,k),k=1,7),(fbc(i,k),k=1,6)
      enddo
      return
      end
 
 
 
 
 
 
 
