.SUFFIXES: .F .o

OBJS = ice_comp_mct.o \
       ice_communicator.o \
       mpsi_cpl_indices.o \
       mpsi_mct_vars.o

#OCEAN_SHARED_INCLUDES=-I../core_ocean/mode_forward -I../core_ocean/shared -I../core_ocean/analysis_members -I../core_ocean/cvmix -I../framework -I../operators
SEAICE_SHARED_INCLUDES=-I../core_cice -I../framework -I../operators

all: $(OBJS)

ice_comp_mct.o: ice_communicator.o mpsi_cpl_indices.o mpsi_mct_vars.o

ice_communicator.o:

mpsi_cpl_indices.o:

mpsi_mct_vars.o:

clean:
	$(RM) *.o *.mod *.f90

.F.o:
	$(FC) -c $(CPPFLAGS) $(CPPINCLUDES) $(FFLAGS) $(FCINCLUDES) $(FREEFLAGS) $(SEAICE_SHARED_INCLUDES) $<

#.F.o:
#	$(RM) $@ $*.mod
#	$(CPP) $(CPPFLAGS) $(CPPINCLUDES) $< > $*.f90
#	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES) -I../framework -I../core_$(CORE) -I../external/esmf_time_f90
