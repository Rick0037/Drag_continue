      subroutine read_control(NMAX,RRHSMAX,REVMax,T0,DT,T,TMAX,NEWMAT,
     +ieig,isteady,NEig,shift, Lcontrol,
     +Fcontrol,NDecompose,PlotTime,kickMe,Nvf)
 
      Implicit none
 
      integer NMAX,ieig,isteady,NEig,Lcontrol,NDecompose,Nvf
      real*8 RRHSMAX,REVMax,T0,DT,T,TMAX, shift,
     +PlotTime

      logical NEWMAT
      character*80 Fcontrol
      logical kickMe
 
C   read from control
 
      OPEN(UNIT=Lcontrol,FILE=Fcontrol,STATUS='OLD')
      read(Lcontrol,*)
      read(Lcontrol,*)  T0
      read(Lcontrol,*)  dt         ! Time increment
      read(Lcontrol,*)  Tmax
      read(Lcontrol,*)  NDecompose ! Each N-th time step decompose
      read(Lcontrol,*)  PlotTime   ! Plot at this and n*this time
      read(Lcontrol,*)   NMAX
      read(Lcontrol,*)   RRHSMAX
      read(Lcontrol,*)   REVMax
      read(Lcontrol,*)   ieig
      read(Lcontrol,*)   isteady
      read(Lcontrol,*)   NEig
      read(Lcontrol,*)   shift
      read(Lcontrol,*)   kickMe
      read(Lcontrol,*)   Nvf
      close(Lcontrol)
 
      shift=  (shift * 2.0D0 * 3.14159D0)**2  !  shift= (nHz * 2 pi ) ^ 2
 
 
C   read from control
 
      return
      end
 
 
 
 
 
 
 
