.SSUFFIXES: .o .f .f90
.f.o: ; $(F77) -c $<

OBJ1 = MF32TEC.f90

OBJ2 = MF32VTK.f90


#
# DNS related variables
#

all:	MF3VTK MF3TEC

MF3TEC:	$(OBJ1) 
	$(F77) $(FFLAGS) -o MF3TEC $(OBJ1) 


MF3VTK:	$(OBJ2) 
	$(F77) $(FFLAGS) -o MF3VTK $(OBJ2) 




