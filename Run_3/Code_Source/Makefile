.SSUFFIXES: .o .f
.f.o: ; $(F77) $(FFLAGS) -c $<


CSR  =../LIBRARY/libsplib.a
ALGS =../LIBRARY/libalgs.a
BLAS =../LIBRARY/libblas.a
DEIS =../LIBRARY/libdeis.a
LAPK =../LIBRARY/liblapk.a
LINP =../LIBRARY/liblinp.a
MISC =../LIBRARY/libmisc.a
SEIS =../LIBRARY/libseis.a
LIBS =  $(CSR) $(ALGS) $(LAPK) $(MISC) $(LINP) $(BLAS) 



OBJ1  = x_dusys.o x_skset.o x_dsplib.o
OBJ2  = eigenvalues.o 
OBJ33 = x_m_v_t3.o x_read_mesh.o x_read_bc.o x_read_control.o x_read_restart.o x_update_unsteady.o x_set_bcV.o x_read_material.o x_assemble_matrix.o x_set_bc_RD.o x_update_internal.o  x_resid.o x_load.o x_set_kick.o

OBJ35 = x_flow10.o x_flow_tria_uns10.o x_flow_tria_ste10.o x_flow6.o x_flow_tria_uns6.o x_flow_tria_ste6.o 

OBJ36 = x_m_p.o 
OBJ34 = xActuator.o 

OBJ77 = A-README_FIRST CHANGES ndf.inc parameter.inc parameter.inc-small parameter.inc-large parameter.inc-huge plot_dir.inc \
        m_v_t3.f read_mesh_bc.f\
        read_control.f read_restart.f update_unsteady.f set_bcV.f read_material.f assemble_matrix.f load.f set_bc_RD.f \
        update_internal.f eigen.f resid.f
OBJ88 = forV3mpeg  mf32tecplot.f hexa.f  img2tiff  NOdialPE.f  NOdials.c  parameter.inc   pal_rgb1.col forV3 img2ps  Makefile  NOdialPE.o  NOdials.o  pltprmfem.000

#
# DNS related variables
#



UNS3:	$(OBJ1) $(OBJ34) $(OBJ33) $(OBJ35) $(OBJ37) $(OBJ36)
	$(F77) $(FFLAGS) -o UNS3 $(OBJ1) $(OBJ33) $(OBJ34)  $(OBJ35) $(OBJ36) $(OBJ37) $(LIBS)
		
clean:	
	-{ rm -f *.o fort.60 fort.61 res.out  Code_Output/*  UNS3   *~ Code_Input/*~ ;}

