#!/bin/csh

# For now, manually build the namelist. Soon this will call the standard ACME
# build-namelist script.

mkdir -p $CASEROOT/CaseDocs

# Put input .nc file and graph.info.part files into the run directory manually
echo "NOTE: You must copy the input .nc file and graph.info.part files into the run directory manually"
#echo $CASEROOT

set MPAS_NML = $CASEROOT/CaseDocs/mpasli_in
touch $MPAS_NML
chmod 644 $MPAS_NML

if ($CONTINUE_RUN == 'TRUE') then
    set config_do_restart = .true.
    set config_start_time = 'file'
#    #TODO - config_start_time must not be read in - but obtained from the coupler
else
    set config_do_restart = .false.
    set config_start_time = '0001-01-01_00:00:00' 
endif

cat >! $MPAS_NML << EOF
&velocity_solver
    config_velocity_solver = 'sia'
!    config_sia_tangent_slope_calculation = 'from_vertex_barycentric'
!    config_flowParamA_calculation = 'constant'
!    config_do_velocity_reconstruction_for_external_dycore = .false.
/

&advection
    config_thickness_advection = 'fo'
    config_tracer_advection = 'none'
/

&physical_parameters
!    config_ice_density = 910.0
!    config_ocean_density = 1028.0
!    config_sea_level = 0.0
!    config_default_flowParamA = 3.1709792e-24
!    config_enhancementFactor = 1.0
!    config_flowLawExponent = 3.0
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
!    config_calendar_type = 'gregorian_noleap'
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
!    config_always_compute_fem_grid = .false.
/
EOF

/bin/cp $CASEROOT/CaseDocs/mpasli_in $RUNDIR



set STREAM_NAME = "streams.landice"
set MPAS_STREAMS = $RUNDIR/$STREAM_NAME
touch $MPAS_STREAMS
chmod 644 $MPAS_STREAMS

# Write streams file
	cat >! $MPAS_STREAMS << 'EOF'
	<streams>

	<immutable_stream name="basicmesh"
					  type="none"
					  filename_template="not-to-be-used.nc"
	/>

	<immutable_stream name="input"
					  type="input"
				  filename_template="landice_grid.nc"
					  input_interval="initial_only"/>

	<!--
	The restart stream is actually controlled via the coupler.
	Changing output_interval here will not have any affect on
	the frequency restart files are written.

	Changing the output_interval could cause loss of data.

	The output_interval is set to 1 second to ensure each restart frame has a
	unique file.
	-->
	<immutable_stream name="restart"
					  type="input;output"
					  filename_template="rst.glc.$Y-$M-$D_$h.$m.$s.nc"
					  filename_interval="output_interval"
					  reference_time="0000-01-01_00:00:00"
					  clobber_mode="truncate"
					  input_interval="initial_only"
					  output_interval="10-00-00_00:00:00"/>

	<!--
	output is the main history output stream. You can add auxiliary streams to
	this stream to include more fields.
	-->

	<stream name="output"
			type="output"
			filename_template="hist.glc.$Y-$M-$D_$h.$m.$s.nc"
			filename_interval="01-00-00_00:00:00"
			reference_time="0000-01-01_00:00:00"
			clobber_mode="truncate"
			output_interval="00-01-00_00:00:00">

    <stream name="basicmesh"/>
    <var_array name="tracers"/>
    <var name="xtime"/>
    <var name="thickness"/>
    <var name="layerThickness"/>
    <var name="lowerSurface"/>
    <var name="upperSurface"/>
    <var name="cellMask"/>
    <var name="edgeMask"/>
    <var name="vertexMask"/>
    <var name="normalVelocity"/>
    <var name="uReconstructX"/>
    <var name="uReconstructY"/>

</stream>

</streams>

'EOF'
endif # Writing streams file

