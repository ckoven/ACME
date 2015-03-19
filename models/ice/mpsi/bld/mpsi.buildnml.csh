#!/bin/csh

# For now, manually build the namelist. Soon this will call the standard CESM
# build-namelist script.

mkdir -p $CASEROOT/CaseDocs

/bin/cp $CODEROOT/ice/mpsi/bld/m120/mpas-cice.graph.info.part.* $CASEROOT/Buildconf/

# MNL - the mpas grid file (ocean120km.nc) is 51M so I don't want to add it to
# the repository. I think it should go in the inputdata repository, but we can
# figure that out later. For now, don't forget to copy ocean120km.nc from
# ~mlevy/MPAS-CESM_files/ to $CASEROOT/Buildconf
echo "NOTE: You must copy ocean120km.nc into $CASEROOT/Buildconf"

set MPAS_NML = $CASEROOT/CaseDocs/mpsi.in
touch $MPAS_NML
chmod 644 $MPAS_NML

if ($CONTINUE_RUN == 'TRUE') then
    set config_do_restart = .true.
    set config_start_time = 'file'
	set config_set_restingThickness_to_IC = .false.
	set config_alter_ICs_for_pbcs = .false.
    #TODO - config_start_time must not be read in - but obtained from the coupler
else
    set config_do_restart = .false.
    set config_start_time = '0001-01-01_00:00:00' 
	set config_set_restingThickness_to_IC = .true.
	set config_alter_ICs_for_pbcs = .true.
endif

cat >! $MPAS_NML << EOF
&cice_model
	config_dt = 3600.0
	config_calendar_type = '360day'
	config_start_time = '$config_start_time'
	config_stop_time = 'none'
	config_run_duration = '0000_01:00:00'
	config_num_halos = 2
/
&io
	config_input_name = '/usr/projects/climate/akt/ACME/input_files/grid_gx1.nc'
	config_output_name = '$CASE.mpsi.hi.nc'
	config_restart_name = '$CASE.mpsi.hi.nc'
	config_forcing_name = 'forcing.nc'
	config_output_interval = '06:00:00'
	config_frames_per_outfile = 0
	config_pio_num_iotasks = 0
	config_pio_stride = 1
	config_restart_timestamp_name = 'restart_timestamp'
	config_test_case_diag = .false.
	config_test_case_diag_type = 'none'
/
&decomposition
	config_block_decomp_file_prefix = '/usr/projects/climate/akt/ACME/input_files/graph.info.part.'
	config_number_of_blocks = 0
	config_explicit_proc_decomp = .false.
	config_proc_decomp_file_prefix = '/usr/projects/climate/akt/ACME/input_files/graph.info.part.'
/
&restart
	config_do_restart = $config_do_restart
	config_restart_interval = 'none'
/
&initialize
	config_initial_condition_type = 'uniform'
	config_initial_ice_area = 0.0
	config_initial_ice_volume = 0.0
	config_initial_snow_volume = 0.0
	config_initial_latitude_north = 0.0
	config_initial_latitude_south = 0.0
	config_initial_velocity_type = 'uniform'
	config_initial_uvelocity = 0.0
	config_initial_vvelocity = 0.0
/
&use_sections
	config_use_velocity_solver = .false.
	config_use_advection = .false.
	config_use_forcing = .false.
	config_use_vertical_thermodynamics = .false.
/
&unit_test
	config_perform_unit_test = .false.
	config_unit_test_type = ''
	config_unit_test_subtype = ''
/
&velocity_solver
	config_rotate_cartesian_grid = .true.
	config_elastic_subcycle_number = 120
	config_stress_divergence_scheme = 'weak'
	config_variational_basis = 'wachspress'
	config_evp_damping = .true.
/
&advection
	config_convert_volume_to_thickness = .true.
	config_limit_ice_concentration = .true.
	config_conservation_check = .true.
	config_clean_tracers = .true.
	config_vert_tracer_adv = 'stencil'
	config_vert_tracer_adv_order = 2
	config_horiz_tracer_adv_order = 2
	config_coef_3rd_order = 0.25
	config_monotonic = .true.
/
EOF

/bin/cp $CASEROOT/CaseDocs/mpsi.in $RUNDIR
