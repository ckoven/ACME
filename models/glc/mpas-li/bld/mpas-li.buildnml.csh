#!/bin/csh

# For now, manually build the namelist. Soon this will call the standard ACME
# build-namelist script.

mkdir -p $CASEROOT/CaseDocs

# Put input .nc file and graph.info.part files into the run directory manually
echo "NOTE: You must copy the input .nc file and graph.info.part files into the run directory manually"

set MPAS_NML = $CASEROOT/CaseDocs/mpasli.in
touch $MPAS_NML
chmod 644 $MPAS_NML

if ($CONTINUE_RUN == 'TRUE') then
    set config_do_restart = .true.
#    set config_start_time = 'file'
#    #TODO - config_start_time must not be read in - but obtained from the coupler
else
    set config_do_restart = .false.
#    set config_start_time = '0001-01-01_00:00:00' 
endif

cat >! $MPAS_NML << EOF
&velocity_solver
    config_velocity_solver = 'sia'
    config_sia_tangent_slope_calculation = 'from_vertex_barycentric'
    config_flowParamA_calculation = 'constant'
    config_do_velocity_reconstruction_for_external_dycore = .false.
/

&advection
    config_thickness_advection = 'fo'
    config_tracer_advection = 'none'
/

&physical_parameters
    config_ice_density = 910.0
    config_ocean_density = 1028.0
    config_sea_level = 0.0
    config_default_flowParamA = 3.1709792e-24
    config_enhancementFactor = 1.0
    config_flowLawExponent = 3.0
    config_dynamic_thickness = 100.0
/

&time_integration
    config_dt = '0001-00-00_00:00:00'
    config_time_integration = 'forward_euler'
/

&time_management
    config_do_restart = $config_do_restart
    config_start_time = '$config_start_time'
    config_stop_time = 'none'
    config_run_duration = '0010_00:00:00'
    config_calendar_type = 'gregorian_noleap'
/

&io
    config_stats_interval = 0
    config_write_stats_on_startup = .false.
    config_stats_cell_ID = 1
    config_write_output_on_startup = .true.
    config_pio_num_iotasks = 0
    config_pio_stride = 1
    config_year_digits = 4
/

&decomposition
    config_num_halos = 3
    config_block_decomp_file_prefix = 'graph.info.part.'
    config_number_of_blocks = 0
    config_explicit_proc_decomp = .false.
    config_proc_decomp_file_prefix = 'graph.info.part.'
/

&debug
    config_print_thickness_advection_info = .false.
    config_always_compute_fem_grid = .false.
/
EOF

/bin/cp $CASEROOT/CaseDocs/mpasli.in $RUNDIR
