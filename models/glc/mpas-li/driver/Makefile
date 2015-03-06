.SUFFIXES: .F .o

OBJS = glc_comp_mct.o \
       glc_communicator.o \
       glc_cpl_indices.o \
       glc_mct_vars.o \
       glc_import_export.o

LANDICE_SHARED_INCLUDES=-I../core_landice -I../framework -I../operators -I../registry/ezxml

all: $(OBJS)

glc_comp_mct.o: glc_communicator.o glc_cpl_indices.o glc_mct_vars.o glc_import_export.o

glc_communicator.o:

glc_cpl_indices.o:

glc_mct_vars.o:

glc_import_export.o:

clean:
	$(RM) *.o *.mod *.f90

.F.o:
	$(FC) -c $(CPPFLAGS) $(CPPINCLUDES) $(FFLAGS) $(FCINCLUDES) $(FREEFLAGS) $(LANDICE_SHARED_INCLUDES) $<

#.F.o:
#	$(RM) $@ $*.mod
#	$(CPP) $(CPPFLAGS) $(CPPINCLUDES) $< > $*.f90
#	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES) -I../framework -I../core_$(CORE) -I../external/esmf_time_f90

