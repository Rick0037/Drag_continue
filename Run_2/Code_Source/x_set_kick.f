       subroutine set_kick(T,kickMe,NbcV,KBC,VBC,x,ndm,NDEmax,NDF,
     .                                      isteady)   
           

       implicit none
       
C     Boundary conditions v
      integer I,J,L, NbcV,NDEmax,NDF,ndm,isteady
      double precision VBC(NDEmax,NDF),T
      double precision x(ndm,NDEmax)
      integer KBC(NDEmax,NDF+1)
      logical kickMe
           
C    No kicks for steady solutions ....      
      
      if(isteady.ne.1) then
      
      
      do   J=1,NbcV
         L = KBC(J,1)
*        create a kick for unsteadyness at the top of the cylinder
*        set true-condition not reachable if you would not like a kick
         if(              x(1,L).gt.(-0.01) .AND. x(1,L) .lt.0.01
     &       .AND. x(2,L) .gt.0.49 .AND.  x(2,L) .lt.0.51 ) then
         if(kickMe .and. T.gt.0.5.and.T.lt.1.0) then  ! KICK !!!
	 
            VBC(J,1) = 0.0 ! Vx velocity
            VBC(J,2) = 3.0 ! Vy velocity
	    write(*,*) 'uns3: kick for unsteady; t=', T
            print*, L ,'----------------------------', VBC(J,1),VBC(J,2)
         else
            VBC(J,1) = 0.0 ! Vx velocity
            VBC(J,2) = 0.0 ! Vy velocity
         end if
      end if
      
      enddo
      
      endif
      
      
      
      return

      end
	 

      
      
