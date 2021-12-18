SOURCE=Code_Source
export F77 = gfortran


export FC  = $(F77)
export FFLAGS =-O2 -fno-automatic -fbounds-check -w -mcmodel=medium 
export LOCAL_PATH=$(PWD)
export QMRPACK_PATH=$(shell find ..  -name PinBall_Adds)

all:	
	@(mkdir -p LIBRARY)
ifneq ($(QMRPACK_PATH),)
	$(MAKE) libs
endif
	cd $(LOCAL_PATH)/$(SOURCE); $(MAKE) UNS3; mv UNS3 $(LOCAL_PATH) ; cd $(LOCAL_PATH)/V3 ; make all ; cd $(LOCAL_PATH) 

libs:	
	cd $(QMRPACK_PATH)/QMRPACK ; ./Setup ; make all ; cp ../LIBRARY/* $(LOCAL_PATH)/LIBRARY ; cd $(LOCAL_PATH);


clean:	
	@(rm -f res.out Control_Barrier  Code_Output/* UNS3 octave-workspace  Control_Output.dat Control_Input.dat *~ Code_Input/*~ Code_Input/Restart_unsteady_T V3/MF3TEC V3/MF3VTK V3/TEC.plt V3/UNS3.vtk V3/tecplot.phy ; cd $(SOURCE); rm -f *.o ; cd $(LOCAL_PATH) ; cd $(QMRPACK_PATH)/QMRPACK ; make clean)


lib_clean:	
	rm -f LIBRARY/* ;

dist_clean:
	$(MAKE)	clean lib_clean
